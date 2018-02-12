
# TODO : Maybe the name "drift" is not correct here. We will measure the
# difference in AUC based on each variable. The score (from 0.5 to 1) will
# represent the drift level of the variable.

#' @export
drift_filter <- function(dt1, dt2 = NULL, by_copy = T) {
  drift_detection <- drift_detector(dt1, dt2)
  drift_column_list <- drift_decision(drift_detection)$keep
  if(by_copy) {
    # Returns a copy of dt1
    return(dt1[, (drift_column_list), with=F])
  } else {
    # Modifies dt1 and returns a pointer to it
    ndt1 <- copy(names(dt1))
    discard <- ndt1[!ndt1 %in% drift_column_list]
    for(col in discard) {
      dt1[, (col) := NULL]
    }
    return(dt1)
  }
}

#' @export
drift_detector <- function(dt1, dt2 = NULL) {
  # Controls
  if(!is.data.table(dt1)) {
    my_log(ctxt = "drift_detector", "requires a data.table")
    stop("drift_detector requires a data.table")
  }
  if(names(dt1) != names(dt2)) {
    cols <- c(setdiff(names(dt2), names(dt1)), setdiff(names(dt1), names(dt2)))
    warning(paste("drift_detector : these colnames are specific to one",
                  "of the data.tables :", cols))
  }
  # Make a copy (this is a detector, not a destructor)
  dt1 <- copy(dt1)
  dt2 <- copy(dt2)
  # Controls
  if("I_position" %in% names(dt1)) {
    stop("drift_detector : I_position is a forbidden name, sorry")
  }
  if(!is.data.table(dt1)) {
    stop("drift_detector : expected a data.table")
  }
  # Creating target variable
  if(is.null(dt2)) {
    my_log(ctxt = "drift_detector", "creating target rank")
    # dt1[, I_position := .I / .N]
    n <- nrow(dt1)
    set(dt1, j = "I_position", value = (1:n)/n)
  } else {
    my_log(ctxt = "drift_detector", "creating target classif")
    if(!is.data.table(dt2)) {
      stop("drift_detector_ranking : expected a data.table for dt2")
    }
    if(nrow(dt1) <= 0 | nrow(dt2) <= 0) {
    my_log(ctxt = "drift_detector", "dimension of dt1 (",
           paste(class(dt1), collapse = ", "), "): ",
           paste(dim(dt1), collapse = ", "))
    my_log(ctxt = "drift_detector", "dimension of dt2 (",
           paste(class(dt1), collapse = ", "), "): ",
           paste(dim(dt2), collapse = ", "))
      stop("drift_detector_ranking : expected non-empty data.tables (0 rows)")
    }
    # dt1[, I_position := 0L]
    # dt2[, I_position := 1L]
    set(dt1, j = "I_position", value = factor(0L, levels = c(0L, 1L)))
    set(dt2, j = "I_position", value = factor(1L, levels = c(0L, 1L)))
    dt1 <- rbindlist(list(dt1, dt2), fill = T)
  }
  # Creating model
  col_iter <- column_iterator(dt_source = dt1, target_colname = "I_position")
  model_list <- foreach(dt_i = col_iter) %do% {
    my_print("drift_detector", mesg = paste("Computing", names(dt_i)[[1]]))
    fitControl <- trainControl(method = "cv",
                               number = 5,
                               preProcOptions = c("center", "scale"))
    myGrid <- expand.grid(nrounds = 20,
                          max_depth = 2,
                          eta = 0.2,
                          gamma = 1,
                          colsample_bytree = 1,
                          min_child_weight = 10,
                          subsample = 0.7)
    fit <- train(I_position ~ .,
                 data = dt_i,
                 method = "xgbTree",
                 trControl = fitControl,
                 tuneGrid = myGrid)
    list(type = ifelse(is.null(dt2), "self", "train vs test"),
         model = fit,
         name = names(dt_i)[[1]],
         perf = fit$results)
  }
  my_log(ctxt = "drift_detector", "all models where created")
  return(model_list)
}

#' @export
drift_decision <- function(drift_detection,
                           Kappa = getOption("mlprepr.default_Kappa_max"),
                           RMSE = getOption("mlprepr.default_RMSE_min"),
                           verbose = T) {
  perfs <- sapply(drift_detection, function(x) c(column = x$name, x$perf))
  perfs <- data.table(t(perfs))
  if("Kappa" %in% names(perfs)) {
    if(verbose) print(perfs[, .(column, Kappa)])
    cols_to_keep <- unlist(perfs[Kappa <= 0.5, column])
    cols_to_kill <- setdiff(unlist(perfs$column), cols_to_keep)
  } else if ("RMSE" %in% names(perfs)) {
    if(verbose) print(perfs[, .(column, RMSE)])
    cols_to_keep <- unlist(perfs[RMSE >= 0.2, column])
    cols_to_kill <- setdiff(unlist(perfs$column), cols_to_keep)
  } else {
    stop("drift_decision : unexpected 'drift_detection' input")
  }
  return(list(keep = cols_to_keep, discard = cols_to_kill))
}

#' @export
drift_print <- function(
  drift_detection,
  default_Kappa_max = getOption("mlprepr.default_Kappa_max"),
  default_RMSE_min = getOption("mlprepr.default_RMSE_min"),
  return_table = F) {
  perfs <- sapply(drift_detection, function(x) c(column = x$name, x$perf))
  perfs <- data.table(t(perfs))
  if("Kappa" %in% names(perfs)) {
    set(perfs, j = "is_drift", value = perfs[["Kappa"]] >= default_Kappa_max)
    set(perfs, j = "is_safe", value = !perfs[["is_drift"]])
    perfs <- perfs[, c("column", "Kappa", "is_drift", "is_safe"), with=F]
  } else if ("RMSE" %in% names(perfs)) {
    set(perfs, j = "is_drift", value = perfs[["RMSE"]] <= default_RMSE_min)
    set(perfs, j = "is_safe", value = !perfs[["is_drift"]])
    perfs <- perfs[, c("column", "RMSE", "is_drift", "is_safe"), with=F]
  } else {
    stop("drift_print : unexpected input")
  }
  if(return_table) {
    return(perfs)
  } else {
    print(perfs)
    invisible()
  }
}

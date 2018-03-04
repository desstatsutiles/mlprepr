

# Note : earthmover tends to ignore rare events and focus on the changes
# of the common values, while kldivergence tends to focus on rare events
# that suddenly become common. As a result, kldivergence can be seens
# as a measure of surprise while earthmover can be seen as a measure
# of overall change. Both can lead to model error though.

###############################################################################
# Drift main function ---------------------------------------------------------
###############################################################################

#' @export
drift_detector <- function(dt1, dt2 = NULL,
                           custom_drift_column_name = NA,
                           sample_if_bigger = 100000,
                           method = "earthmover") {
  if(!is.na(custom_drift_column_name) & !is.null(dt2)) {
    stop("Not implemented : when dt2 is not null we would still use classif")
  }
  # Controls ------------------------------------------------------------------
  if(!is.data.table(dt1)) {
    my_log(ctxt = "drift_detector", "requires a data.table")
    stop("drift_detector requires a data.table")
  }
  if(is.data.table(dt2)) {
    cols <- c(setdiff(names(dt2), names(dt1)), setdiff(names(dt1), names(dt2)))
    if(length(cols) > 0) {
      warning(paste("drift_detector : these colnames are specific to one",
                    "of the data.tables :", cols))
    }
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
  # Creating target variable --------------------------------------------------
  if(is.null(dt2)) {
    # Force method == xgbtree, others dont work here
    if(method != "xgbtree") {
      stop("drift_detector : please use 'method = xgbtree' when dt2 is NULL")
    }
    # Creating target variable (for dt1 only) - - - - - - - - - - - - - - - - -
    if(is.na(custom_drift_column_name)) {
      # Creating a column 1..n as the target
      my_log(ctxt = "drift_detector", "creating target rank")
      n <- nrow(dt1)
      set(dt1, j = "I_position", value = (1:n)/n)
    } else {
      # Renaming an existing column, that will be the target
      # Note : has to be an int, and the prediction will be a regression
      my_log(ctxt = "drift_detector", "renaming custom target")
      if(!is.character(custom_drift_column_name)) {
        stop("drift_detector : expected a character for custom_colname")
      }
      setnames(dt1, old = custom_drift_column_name, new = "I_position")
    }
  } else {
    # Creating target variable (for dt1 & dt2) - - - - - - - - - - - - - - - -
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
  # Sampling to reduce computation time
  if(!is.na(sample_if_bigger) & sample_if_bigger <= nrow(dt1)) {
    my_sample <- sample(1:nrow(dt1), size = sample_if_bigger)
    dt1 <- dt1[my_sample, ]
  }
  # Imputation : prevent error caused by missing data -------------------------
  encode_nas(dt1)
  # Converting to numeric
  # NB : this is not required to work, but it will cause factors to be encoded
  # in an order corresponding to their number of observed rows, which is better
  # for earthmover's distance
  drift_to_numeric(dt1)
  # Creating model ------------------------------------------------------------
  col_iter <- column_iterator(dt_source = dt1, target_colname = "I_position")
  model_list <- foreach(dt_i = col_iter) %do% {
    my_print("drift_detector", mesg = paste("Computing", names(dt_i)[[1]]))
    drift_one_col(dt_i, is.null(dt2), method = method)
  }
  my_log(ctxt = "drift_detector", "all models where created")
  return(model_list)
}

###############################################################################
# Drift data preparation functions --------------------------------------------
###############################################################################

# Turn a data.table to numeric
# This is not reproductible
# It only works because we do in on ONE SINGLE table
drift_to_numeric <- function(dt) {
  my_log(ctxt = "drift_to_numeric", "converting columns to numeric")
  ndt_loop <- copy(names(dt))[copy(names(dt)) != "I_position"]
  for(ndt in ndt_loop) {
    nidt <- which(names(dt) == ndt)
    col <- dt[[nidt]]
    if(!is.numeric(col)) {
      if(is.character(col)) {
        col <- factor(col)
      }
      if(is.factor(col)) {
        old_lvl <- names(sort(table(col)))
        new_lvl <- 1:length(old_lvl)
        x <- paste(old_lvl, collapse = ",")
        y <- paste(new_lvl, collapse = ",")
        my_log(ctxt = "drift_to_numeric",
               paste("converting", ndt, "from", x, "to", y))
        col <- plyr::mapvalues(col, from = old_lvl, to = new_lvl)
        set(dt, j = nidt, value = as.integer(col))
      } else if(is.logical(col)) {
        set(dt, j = nidt, value = as.integer(col))
      } else {
        warning("drift_to_numeric : Unexpected type")
      }
    }
  }
  my_log(ctxt = "drift_to_numeric", "convertion done")
}

###############################################################################
# Drift measure for one column functions --------------------------------------
###############################################################################

drift_one_col <- function(dt_i, method = "xgbtree", self = F) {
  # Note : some may require numeric only data
  if(method == "xgbtree") {
    res <- drift_one_col_xgbTree(dt_i, self)
  } else if (method == "kldivergence") {
    res <- drift_one_col_kldivergence(dt_i, self)
  } else if (method == "earthmover") { # ie : EM / Wasserstein / Mallow's
    res <- drift_one_col_earthmover(dt_i, self)
  }
  # Set mean and stdev
  res$mean_0 <- mean(dt_i[I_position == 0L][[1]])
  res$mean_1 <- mean(dt_i[I_position == 1L][[1]])
  res$sd_0 <- sd(dt_i[I_position == 0L][[1]])
  res$sd_1 <- sd(dt_i[I_position == 1L][[1]])
  res$delta_mean <- abs(res$mean_0 - res$mean_1) / mean(c(res$mean_0, res$mean_1)) *100
  res$delta_sd <- abs(res$sd_0 - res$sd_1) / mean(c(res$sd_0, res$sd_1)) *100
  res$q1_0 <- as.vector(quantile(dt_i[I_position == 0L][[1]], probs = 1/10))
  res$q1_1 <- as.vector(quantile(dt_i[I_position == 1L][[1]], probs = 1/10))
  res$q9_0 <- as.vector(quantile(dt_i[I_position == 0L][[1]], probs = 9/10))
  res$q9_1 <- as.vector(quantile(dt_i[I_position == 1L][[1]], probs = 9/10))
  res$delta_q1 <- abs(res$q1_0 - res$q1_1) / mean(c(res$q1_0, res$q1_1)) *100
  res$delta_q9 <- abs(res$q9_0 - res$q9_1) / mean(c(res$q9_0, res$q9_1)) *100
  # Return
  return(res)
}

# Splits a numeric vector into k bins
# Each bin has the same number of elements (or close to), but not necessarily
# the same size (i.e. width) or position.
# Example of the influence of n (number of bins) on drift
#    n        kl1        kl2         em       em/n
#    5 0.03222391 0.03566394  0.2777723 0.05555446
#   10 0.03815856 0.04520001  0.5500302 0.05500302
#   25 0.04245320 0.05436312  1.3510633 0.05404253
#   50 0.04424062 0.05987107  2.6734111 0.05346822
#   75 0.04435225 0.06019752  4.0623918 0.05416522
#  100 0.04501572 0.06199279  5.3855219 0.05385522
#  200 0.04631831 0.06426815 10.7415876 0.05370794
# As you can see, kl is almost invariant in n and em/n is
drift_bin <- function(vec, k = 37) {
  if(length(unique(vec)) > k) {
    my_log(ctxt = "drift_bin", "splitting column...")
    cuts <- cut2(vec, m = length(vec)/k)
    levels(cuts) <- 1:length(levels(cuts))
    return(as.numeric(cuts))
  }
  return(vec)
}

drift_get_counts <- function(dt_i) {
  colname <- copy(names(dt_i))[1]
  # Split column in bins
  set(dt_i, j = colname, value = drift_bin(dt_i[[1]]))
  # Compute sum(0) and sum(1) per bin
  counts <- dt_i[, .(Z = sum(I_position == 0L), U = sum(I_position == 1L)), keyby = colname]
  # Convert counts to percents
  set(counts, j = "Z", value = counts$Z / sum(counts$Z))
  set(counts, j = "U", value = counts$U / sum(counts$U))
  return(counts)
}

# Attention, si 2 variables ont un nombre différent de modalités,
# on ne peut pas comparer leurs emd puisque le max sur 2 modas est de 1,
# sur 3 modas de 2, et ainsi de suite !
drift_one_col_earthmover <- function(dt_i, self) {
  if(self) {
    stop("drift_one_col_earthmover : self should be F, not implemented")
  } else {
    counts <- drift_get_counts(dt_i)
    em <- emdist::emd2d(as.matrix(counts$Z), as.matrix(counts$U))
    res <- list(type = ifelse(self, "self", "train vs test"),
                model = "em",
                name = names(dt_i)[[1]],
                perf_original = em,
                perf = em / length(counts$Z)) # EMB / nb de bins
    return(res)
  }
}

drift_one_col_kldivergence <- function(dt_i, self = F) {
  if(self) {
    stop("drift_one_col_kldivergence : self should be F, not implemented")
  } else {
    counts <- drift_get_counts(dt_i)
    if(any(counts$Z == 0) | any(counts$U == 0)) {
      counts_pq <- copy(counts)
      counts_pq[U == 0, U := 1]
      counts_pq[U == 0, Z := 0]
      kl1 <- entropy::KL.empirical(counts_pq$Z, counts_pq$U)
      counts_qp <- copy(counts)
      counts_qp[Z == 0, U := 0]
      counts_qp[Z == 0, Z := 1]
      kl2 <- entropy::KL.empirical(counts_qp$U, counts_qp$Z)
    } else {
      kl1 <- entropy::KL.empirical(counts$Z, counts$U)
      kl2 <- entropy::KL.empirical(counts$U, counts$Z)
    }
  }
  res <- list(type = ifelse(self, "self", "train vs test"),
              model = "kl",
              name = names(dt_i)[[1]],
              perf = max(kl1, kl2),
              perf1 = kl1,
              perf2 = kl2)
  return(res)
}

drift_one_col_xgbTree <- function(dt_i, self = F) {
  fitControl <- trainControl(method = "cv",
                             number = 3)
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
               tuneGrid = myGrid,
               preProcess = c("center", "scale"))
  res <- list(type = ifelse(self, "self", "train vs test"),
              model = fit,
              name = names(dt_i)[[1]],
              perf = fit$results)
  return(res)
}

###############################################################################
# Drift decision and display functions ----------------------------------------
###############################################################################

#' @export
drift_decision <- function(drift_detection,
                           verbose = T) {
  perfs <- drift_print(drift_detection, return_table = T)
  return(list(keep = perfs[is_drift == F, column],
              discard = perfs[is_drift == T, column]))
}

#' @export
drift_print <- function(
  drift_detection,
  default_Kappa_max = getOption("mlprepr.default_Kappa_max"),
  default_RMSE_min = getOption("mlprepr.default_RMSE_min"),
  default_em_max = getOption("mlprepr.default_em_max"),
  default_kl_max = getOption("mlprepr.default_kl_max"),
  return_table = F) {
  if(is.character(drift_detection[[1]]$model)) {
    # Computing perfs for EM and KL -------------------------------------------
    perfs <- sapply(drift_detection, function(x) {
      c(column = x$name,
        perf = x$perf,
        em = x$perf_original,
        kl1 = x$perf1,
        kl2 = x$perf2,
        delta_mean = x$delta_mean,
        delta_sd = x$delta_sd,
        delta_q1 = x$delta_q1,
        delta_q9 = x$delta_q9)})
    perfs <- data.table(t(perfs))
    if(drift_detection[[1]]$model == "em") {
      set(perfs, j = "is_drift", value = perfs[["perf"]] >= default_em_max)
      set(perfs, j = "is_safe", value = !perfs[["is_drift"]])
    } else if (drift_detection[[1]]$model == "kl") {
      set(perfs, j = "is_drift", value = perfs[["perf"]] >= default_kl_max)
      set(perfs, j = "is_safe", value = !perfs[["is_drift"]])
    } else {
      stop("drift_print : unexpected model type")
    }
  } else {
    # Computing perfs for xgbTree ---------------------------------------------
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
}

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
drift_display_variable <- function(dt,
                                   varname,
                                   categoryname = "mois",
                                   title = "insert title here",
                                   extreme = 100,
                                   bw = NA) {
  dt_select <- dt[, .(cat = factor(get(categoryname)), var = get(varname))]
  qmin <- as.integer(quantile(dt_select$var, probs = 1/extreme, na.rm = T))
  qmax <- as.integer(quantile(dt_select$var, probs = (extreme-1)/extreme, na.rm = T))
  if(!is.na(extreme)) {
    # Remove extreme values
    dt_select[var < qmin, var := qmin - sqrt(var(var))]
    dt_select[var > qmax, var := qmax - sqrt(var(var))]
  }
  if(is.na(bw)) {
    max_q <- max(abs(qmin), abs(qmax))
    bw <- max(round(max_q/5), 1)
  }
  dt_plot <- na.omit(dt_select)[sample(1:.N, min(10000, .N))]
  graph <- ggplot2::ggplot(dt_plot, aes(var, ..density.., fill = cat))
  graph <- graph + geom_histogram(binwidth = bw,
                                  alpha = .5,
                                  position = position_dodge(width = bw*.9))
  return(graph + labs(title = title))
}

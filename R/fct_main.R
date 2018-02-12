
# Define learn_transformer parameters -----------------------------------------
#' @export
learn_transformer_parameters <- function(target_colname = "target",
                                         nzv_type = NA,
                                         nzv_freqCut = 100/1,
                                         nzv_uniqueCut = 5,
                                         winsor_min = 0.05,
                                         winsor_max = 0.95,
                                         factor_min_nb_per_level = 100,
                                         factor_max_nb_of_levels = 10,
                                         factor_other_level = "other",
                                         factor_rare_level = "rare") {
  res <- list(
    # Target var parameters
    target_colname = target_colname,
    # Remove zero-variance
    nzv_type = nzv_type,
    nzv_freqCut = nzv_freqCut,
    nzv_uniqueCut = nzv_uniqueCut,
    # Numeric params
    winsor_min = winsor_min,
    winsor_max = winsor_max,
    # Factor params
    # | Min number of times the levels should appear (avoid rare levels)
    factor_min_nb_per_level = factor_min_nb_per_level,
    # | Max number of distinct levels to keep (= nb of dummy vars created)
    factor_max_nb_of_levels = factor_max_nb_of_levels,
    # | Name of factor values that were deleted
    factor_other_level = factor_other_level, # because grouped
    factor_rare_level = factor_rare_level # because rare
  )
  return(res)
}

# Learn a transform based on a data.table and its target column ---------------
#' @export
learn_transformer <- function(dt_source,
                              params = learn_transformer_parameters()) {
  # Remove zero- (or low-) variance columns
  if(!is.na(params$nzv_type)) {
    nzv_result <- nearZeroVar(x = dt_source,
                              freqCut = params$nzv_freqCut,
                              uniqueCut = params$nzv_uniqueCut,
                              saveMetrics = T,
                              names = T,
                              foreach = T,
                              allowParallel = F)
    nzv_result <- data.table(nzv_result, keep.rownames = T)
    if(params$nzv_type == "zeroVar") {
      cols_to_keep <- nzv_result[zeroVar == FALSE, rn]
    } else if (nzv_type == "nzv") {
      cols_to_keep <- nzv_result[nzv == FALSE, rn]
    } else {
      stop("learn_transformer : nzv_type should be 'zeroVar' or 'nzv'")
    }
    if(length(cols_to_keep) < names(dt_source)) {
      keep_col <- names(dt_source) %in% cols_to_keep
      msg_cols <- paste(names(dt_source)[keep_col], collapse = ", ")
      message(paste("learn_transformer : the following columns have been",
                    "discarded because of their zero or low variance :",
                    msg_cols))
      dt_source <- dt_source[, which(keep_col), with=F]
    } else {
      message("learn_transformer : all columns kept, variance is ok.")
    }
  }
  # Compute iterator on columns + target
  iter_c <- column_iterator(dt_source, params$target_colname)
  list_of_transforms <- foreach(col_i = iter_c) %do% {
    col_i_x <- col_i[[1]]
    if(is.factor(col_i_x)) {
      learn_transformer_factor(col_i, params)
    } else if(is.character(col_i_x)) {
      learn_transformer_character(col_i, params)
    } else if(is.numeric(col_i_x)) {
      learn_transformer_number(col_i, params)
    } else if(is.logical(col_i_x)) {
      learn_transformer_logical(col_i)
    } else {
      # Handling not supported types
      col_i_name <- names(col_i)[[1]]
      exemples <- paste(head(col_i_x), collapse = ", ")
      stop(paste0("Found type that is not supported (",
                  col_i_name, ") : ", exemples))
    }
  }
  return(list(
    list_of_transforms = list_of_transforms,
    params = params))
}

learn_transformer_number <- function(col, params) {
  my_log("learn_transformer_number", names(col)[1])
  # Read parameters
  winsor_min <- params$winsor_min
  winsor_max <- params$winsor_max
  # Compter transforms
  col_name <- names(col)[1]
  return(list(
    col_name = col_name,
    transformer = "number",
    winsor = winsor_learn(col[[1]], winsor_min, winsor_max)
  ))
}

learn_transformer_character <- function(col, params) {
  my_log("learn_transformer_character", names(col)[1])
  col_1st_name <- copy(names(col)[1]) # copy isnt required here
  col[,(col_1st_name):=lapply(.SD, as.factor),.SDcols=col_1st_name]
  res <- learn_transformer_factor(col, params)
  return(res)
}

learn_transformer_factor <- function(col, params) {
  col_1st_name <- copy(names(col)[1]) # copy isnt required here
  my_log("learn_transformer_factor", col_1st_name)
  # Read parameters
  target_colname <- params$target_colname
  min_levels <- params$factor_min_nb_per_level
  max_levels <- params$factor_max_nb_of_levels
  # Save original levels
  original_levels <- copy(levels(col[[1]]))
  rare_levels     <- NA # by default
  other_levels    <- NA # by default
  # Only keep levels that are common enough
  new_fac <- GroupModas(col[[1]],
                        min_levels,
                        others_name = params$factor_rare_level)
  new_fac <- RecodeEmptyString(new_fac)
  rare_levels <- setdiff(original_levels, levels(new_fac))
  set(col, j = col_1st_name, value = new_fac)
  if(length(unique(col[[1]])) == 0) {
    # we have deleted all factors (none is common enough)
    # we can return right now
    my_print("learn_transformer_factor",
             paste("ignoring column", col_1st_name, ": 0 levels"))
    return(list(
      col_name = col_1st_name,
      transformer = "ignore"
    ))
  }
  # Only keep levels so that there is no more than k levels
  nb_of_distinct_levels <- length(unique(col[[1]]))
  if(nb_of_distinct_levels > max_levels) {
    my_print("learn_transformer_factor",
             paste("deleting rare levels (",
                   nb_of_distinct_levels, ">", max_levels, ")"))
    # Look for the top-k levels (where k = max_levels)
    col_n_by_level <- col[, .N, by = col_1st_name]
    setkey(col_n_by_level, N)
    ok_lvls <- col_n_by_level[(.N-max_levels+1):.N][, get(col_1st_name)]
    # Convert other levels to "other"
    other_levels <- col[!Class %in% ok_lvls, Class]
    col[!Class %in% ok_lvls, Class := params$factor_other_level]
  }
  if(length(levels(col[[1]])) <= 1) {
    my_print("learn_transformer_factor",
             paste("ignoring column", col_1st_name, ": too few levels"))
    # If there are too few levels, ignore the column
    return(list(
      col_name = col_1st_name,
      transformer = "ignore"
    ))
  } else {
    my_log("learn_transformer_factor",
             paste("computing ohe for", col_1st_name))
    # Learn a one-hot encoder
    my_formula <- formula(paste0("~ ", col_1st_name))
    dmy <- dummyVars(my_formula, data = col, fullRank = T)
    # Return the encoder
    return(list(
      col_name = col_1st_name,
      transformer = "factor",
      onehotencoder = dmy,
      levels_kept = levels(col[[1]]),
      original_levels = original_levels,
      rare_levels = rare_levels,
      other_levels = other_levels
    ))
  }
}

learn_transformer_logical <- function(col, params) {
  my_log("learn_transformer_logical", names(col)[1])
  # We always turn them into 0/1 with "as.integer"
  return(list(
    col_name = copy(names(col)[1]),
    transformer = "logical"
  ))
}

# Learn a transform based on a data.table and its target column ---------------
#' @export
apply_transformer <- function(dt_source,
                              transformer,
                              keep_relevant_columns_only = T) {
  # Warning for reference
  message("apply_transformer will modify dt_source by reference")
  # Extract transformer and params
  tr_transformer <- transformer$list_of_transforms
  tr_params <- transformer$params
  # Keep relevant columns only (i.e. those in tr_transformer)
  if(keep_relevant_columns_only) {
    not_relevant_cols <- setdiff(names(dt_source),
                        c(tr_params$target_colname,
                          sapply(tr_transformer, function (x) x$col_name)))
    if(length(not_relevant_cols) > 0) {
      not_relevant_cols_names <- paste(not_relevant_cols, collapse = ",")
      my_log(ctxt = "apply_transformer",
             mesg = paste("removing", not_relevant_cols_names))
      # dt_source[, (not_relevant_cols) := NULL]
      for(col in not_relevant_cols) set(dt_source, j = col, value = NULL)
    }
  }
  # Compute iterator on columns their corresponding transformer
  source_names <- copy(names(dt_source)) # Get column names by copy
  iter_ct <- column_and_transformer_iterator(source_names, tr_transformer)
  # Use iterator to loop on each column
  o <- foreach(col_i = iter_ct) %do% {
    col_i_name = col_i$col_name
    my_log(ctxt = "apply_transformer",
           paste("starts for", col_i_name,
                 "of type", col_i$transformer))
    # Compute the column(s) resulting from this transform applied to this col
    # Remove column from table
    col_i_old <- dt_source[[col_i_name]]
    set(dt_source, j = col_i_name, value = NULL)
    # Create new columns
    if(col_i$transformer == "number") {
      dt_new_cols <- apply_transformer_number(col_i_old,
                                              col_i_name,
                                              col_i)
    } else if (col_i$transformer == "factor") {
      # Note : characters have transformer set to "factor" too
      dt_new_cols <- apply_transformer_factor(col_i_old,
                                              col_i_name,
                                              col_i,
                                              tr_params)
    } else if (col_i$transformer == "logical") {
      dt_new_cols <- apply_transformer_logical(col_i_old,
                                              col_i_name)
    } else if (col_i$transformer == "ignore") {
      dt_new_cols <- NULL
      my_print("apply_transformer", paste("ignored column", col_i_name))
    } else {
      dt_new_cols <- NULL
      my_print("apply_transformer", paste("unknown type", col_i$transformer))
    }
    # Insert them
    my_log("apply_transformer", mesg = paste(col_i_name, "computed. Then :"))
    if(!is.null(dt_new_cols)) {
      my_log("apply_transformer", mesg = paste("Inserting", col_i_name, "..."))
      cbind_by_reference(dt_source, dt_new_cols)
    } else {
      my_log("apply_transformer", mesg = paste(col_i_name, ": null"))
    }
    # Info
    my_log("apply_transformer", mesg = paste(col_i_name, "done :)"))
    NULL
  }
  return(dt_source)
}

column_and_transformer_iterator <- function(source_names, transformer) {
  tf_names <- sapply(transformer, function(x) x$col_name)
  transformable <- function(x) {
    # transformable checks that x is a column listed in transformer
    res <- x %in% tf_names
    return(res)
  }
  colname_iter <- iter(source_names, checkFunc = transformable)
  nextEl <- function() {
    col_name <- nextElem(colname_iter)
    transformer_id <- which(tf_names == col_name)
    return(transformer[[transformer_id]])
  }
  obj <- list(nextElem=nextEl)
  class(obj) <- c('iforever','abstractiter','iter')
  obj
}

apply_transformer_number <- function(col_old, col_old_name, col_params) {
  # Declare data.table to fill
  dt <- data.table(col_old)
  setnames(dt, col_old_name)
  # Winsor
  col_wins <- winsor_predict(col_old, col_old_name,
                             col_params$winsor$min, col_params$winsor$max)
  set(dt, i = NULL, j = col_old_name, value = col_wins)
  return(dt)
}

apply_transformer_factor <- function(col_old,
                                     col_old_name,
                                     col_params,
                                     params) {
  # Type check
  if(!(is.factor(col_old) | is.character(col_old))) {
    stop("apply_transformer_factor expects a factor or character")
  }
  # If it is in fact a char, convert to factor
  if(is.character(col_old)) col_old <- as.factor(col_old)
  # Then keep relevant levels
  # First, recode "others"
  if(!is.na(col_params$other_levels)) {
    my_log("apply_transformer_factor",
           mesg = paste("recode other =", params$factor_other_level))
    levels_to_change <- col_params$other_levels
    col_old <- mapvalues(
      col_old,
      from = levels_to_change,
      to = rep(params$factor_other_level, length(levels_to_change)))
  }
  # Then, recode unknown levels and rare ones
  my_log("apply_transformer_factor",
         mesg = paste("recode rare =", params$factor_rare_level))
  levels_to_change <-
    levels(col_old)[!levels(col_old) %in% col_params$levels_kept]
  new_levels <- rep(params$factor_rare_level, length(levels_to_change))
  col_old <- mapvalues(
    col_old,
    from = levels_to_change,
    to = new_levels)
  # Finally, create dummy vars
  if("onehotencoder" %in% names(col_params)) {
    my_log("apply_transformer_factor", mesg = "onehotencoder")
    dt_col_old <- data.table(col_old)
    setnames(dt_col_old, col_old_name)
    new_col <- as.data.table(
      predict(col_params$onehotencoder,
              newdata = dt_col_old))
    my_log("apply_transformer_factor", mesg = "onehotencoder ok")
  } else {
    my_log("apply_transformer_factor", mesg = "stop : expected onehotencoder")
    stop("apply_transformer_factor : expected onehotencoder")
  }
  return(new_col)
}

apply_transformer_logical <- function(col_old, col_old_name) {
  # To integer
  val <- as.integer(col_old)
  # Declare data.table to fill
  dt <- data.table(val)
  setnames(dt, col_old_name)
  return(dt)
}

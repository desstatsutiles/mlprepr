
require(data.table)
require(iterators)
require(foreach)
require(caret)
require(reshape2)
require(doParallel)
require(plyr)
source("R/fct_utils.R")
source("R/fct_winsor.R")

# Define learn_transformer parameters -----------------------------------------
learn_transformer_parameters <- function(target_colname = "target",
                                         nzv_type = NA,
                                         nzv_freqCut = 100/1,
                                         nzv_uniqueCut = 5,
                                         winsor_min = 0.05,
                                         winsor_max = 0.95,
                                         factor_min_nb_per_level = 100,
                                         factor_max_nb_of_levels = 10) {

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
    factor_max_nb_of_levels = factor_max_nb_of_levels
  )
  return(res)
}

# Learn a transform based on a data.table and its target column ---------------
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
    my_print("learn_transformer", names(col_i)[1])
    col_i_x <- col_i[[1]]
    if(is.factor(col_i_x)) {
      learn_transformer_factor(col_i, params)
    } else if(is.character(col_i_x)) {
      learn_transformer_character(col_i, params)
    } else if(is.numeric(col_i_x)) {
      learn_transformer_number(col_i)
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
  return(list_of_transforms)
}

column_iterator <- function(dt_source, target_colname = "target") {
  not_target <- function(x) return(x != target_colname)
  colname_iter <- iter(names(dt_source), checkFunc = not_target)
  nextEl <- function() {
    next_colname <- nextElem(colname_iter)
    next_cols <- c(next_colname, target_colname)
    my_log("column_iterator (iter)", mesg = next_colname, type = "message")
    return(dt_source[, (next_cols), with=F])
  }
  obj <- list(nextElem=nextEl)
  class(obj) <- c('iforever','abstractiter','iter')
  obj
}

learn_transformer_number <- function(col) {
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
  col_1st_name <- copy(names(col)[1]) # copy isnt required here
  col[,(col_1st_name):=lapply(.SD, as.factor),.SDcols=col_1st_name]
  res <- learn_transformer_factor(col, params)
  return(res)
}

learn_transformer_factor <- function(col, params) {
  col_1st_name <- copy(names(col)[1]) # copy isnt required here
  my_print("learn_transformer_factor", col_1st_name)
  # Read parameters
  target_colname <- params$target_colname
  min_levels <- params$factor_min_nb_per_level
  max_levels <- params$factor_max_nb_of_levels
  # Only keep levels that are common enough
  factors_to_keep <- as.vector(col[, .N, by = col_1st_name][N >= min_levels, Name])
  col <- col[get(col_1st_name) %in% unique(factors_to_keep)]
  if(nrow(col) == 0) {
    # we have deleted all factors (none is common enough)
    # we can return right now
    return(NULL) # TODO : check that null will be handled
  }
  # Only keep levels so that there is no more than k levels
  nb_of_distinct_levels <- length(levels(as.factor(col[[1]])))
  if(nb_of_distinct_levels > max_levels) {
    my_print("learn_transformer_factor",
             paste("deleting rare levels (",
                   nb_of_distinct_levels, ">", max_levels, ")"))
    # Look for the top-k levels (where k = max_levels)
    col_n_by_level <- col[, .N, by = col_1st_name]
    setkey(col_n_by_level, N)
    ok_lvls <- col_n_by_level[(.N-max_levels+1):.N][, get(col_1st_name)]
    # Convert other levels to "other"
    # TODO : "other" should be a parameter, not hardcoded here
    col[!Class %in% ok_lvls, Class := "other"]
  }
  # Learn a one-hot encoder
  my_formula <- formula(paste0("~ ", col_1st_name))
  dmy <- dummyVars(my_formula, data = col, fullRank = T)
  # Return the encoder
  return(list(
    col_name = col_1st_name,
    transformer = "factor",
    onehotencoder = dmy,
    levels_kept = levels(col[[1]])
  ))
}

learn_transformer_logical <- function(col, params) {
  # We always turn them into 0/1 with "as.integer"
  return(list(
    col_name = copy(names(col)[1]),
    transformer = "logical"
  ))
}

# Learn a transform based on a data.table and its target column ---------------
apply_transformer <- function(dt_source, transformer) {
  # Compute iterator on columns their corresponding transformer
  source_names <- copy(names(dt_source)) # Get column names by copy
  iter_ct <- column_and_transformer_iterator(source_names, transformer)
  # Use iterator to loop on each column
  o <- foreach(col_i = iter_ct) %do% {
    col_i_name = col_i$col_name
    my_log(ctxt = "apply_transformer", col_i_name)
    # Compute the column(s) resulting from this transform applied to this col
    # Remove column from table
    col_i_old <- dt_source[, get(col_i_name)]
    dt_source[, (col_i_name) := NULL]
    # Create new columns
    if(col_i$transformer == "number") {
      dt_new_cols <- apply_transformer_number(col_i_old,
                                              col_i_name,
                                              col_i$winsor$min,
                                              col_i$winsor$max)
    } else if (col_i$transformer == "factor") {
      dt_new_cols <- apply_transformer_factor(col_i_old,
                                              col_i_name,
                                              col_i$onehotencoder,
                                              col_i$levels_kept)
    } else if (col_i$transformer == "logical") {
      dt_new_cols <- apply_transformer_logical(col_i_old,
                                              col_i_name)
    }
    # Insert them
    cbind_by_reference(dt_source, dt_new_cols)
    NULL
  }
  return(dt_source)
}

column_and_transformer_iterator <- function(source_names, transformer) {
  tf_names <- sapply(transformer, function(x) x$col_name)
  transformable <- function(x) {
    res <- x %in% tf_names
    my_log("cati", paste(x, "est-il ok ?", res))
    return(res)
  }
  colname_iter <- iter(source_names, checkFunc = transformable)
  nextEl <- function() {
    col_name <- nextElem(colname_iter)
    my_log("column_and_transformer_iterator (iter)",
           mesg = col_name, type = "message")
    transformer_id <- which(tf_names == col_name)
    return(transformer[[transformer_id]])
  }
  obj <- list(nextElem=nextEl)
  class(obj) <- c('iforever','abstractiter','iter')
  obj
}

apply_transformer_number <- function(col_old, col_old_name, min, max) {
  # Declare data.table to fill
  dt <- data.table(col_old)
  setnames(dt, col_old_name)
  # Winsor
  col_wins <- winsor_predict(col_old, col_old_name, min, max)
  set(dt, i = NULL, j = col_old_name, value = col_wins)
  return(dt)
}

apply_transformer_factor <- function(col_old,
                                     col_old_name,
                                     onehotencoder,
                                     levels_kept) {
  # TODO : assert to make sure it is factor or char, and test params too

  # If it is in fact a char, convert to factor
  if(is.character(col_old)) col_old <- as.factor(col_old)
  # Then keep relevant levels (ie. replace other levels by "other")
  levels_to_change <- levels(col_old)[!levels(col_old) %in% levels_kept]
  # TODO : "other" should be a parameter, not hardcoded here
  mapvalues(col_old,
            from = levels_to_change,
            to = rep("other", length(levels_to_change)))
  # Finally, create dummy vars
}

apply_transformer_logical <- function(col_old, col_old_name) {
  # To integer
  val <- as.integer(col_old)
  # Declare data.table to fill
  dt <- data.table(val)
  setnames(dt, col_old_name)
  return(dt)
}


require(data.table)
require(iterators)
require(foreach)
require(caret)
require(reshape2)
require(doParallel)
source("R/fct_utils.R")
source("R/fct_winsor.R")

# Define learn_transformer parameters -----------------------------------------
learn_transformer_parameters <- function(target_colname = "target") {
  return(list(target_colname = target_colname))
}

# Learn a transform based on a data.table and its target column ---------------
learn_transformer <- function(dt_source,
                              params = learn_transformer_parameters()) {
  # Compute iterator on columns + target
  iter_c <- column_iterator(dt_source, params$target_colname)
  list_of_transforms <- foreach(col_i = iter_c) %do% {
    col_i_x <- col_i[[1]]
    if(is.factor(col_i_x)) {
      learn_transformer_factor(col_i)
    } else if(is.character(col_i_x)) {
      learn_transformer_character(col_i)
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
  col_name <- names(col)[1]
  return(list(
    col_name = col_name,
    transformer = "number",
    winsor = winsor_learn(col[[1]], 0.05, 0.95)
  ))
}
learn_transformer_character <- function() {}
learn_transformer_factor <- function() {}
learn_transformer_logical <- function() {}

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

apply_transformer_character <- function() {}
apply_transformer_factor <- function() {}
apply_transformer_logical <- function() {}

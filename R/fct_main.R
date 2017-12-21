
require(data.table)
require(iterators)
require(foreach)
require(caret)
require(reshape2)
require(doParallel)

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
    next_cols <- c(nextElem(colname_iter), target_colname)
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
  iter_ct <- column_and_transformer_iterator(dt_source, list_of_transforms)
  # Use iterator to loop on each column
  o <- foreach(col_i = iter_ct) %do% {
    # Compute the column(s) resulting from this transform applied to this col
    # Remove column from table
    col_i_old <- dt_source[[col_i$col_name]]
    dt_source[[col_i$col_name]] <- NULL
    # Create new columns
    if(ct_it$transformer == "number") {
      dt_new_cols <- apply_transformer_number(col_i_old, ct_it)
    }
    # Insert them
    oo <- foreach(ci = names(dt_new_cols)) %do% {
      if(ci %in% names(dt_source)) stop("Trying to insert existing column")
      set(dt_source, i = NULL, j = ci, value = dt_new_cols[[ci]])
      NULL
    }
    NULL
  }
  return(dt_source)
}

column_and_transformer_iterator <- function(dt_source, transformer) {
  tf_names <- sapply(transformer, function(x) x$col_name)
  transformable <- function(x) return(x %in% tf_names)
  colname_iter <- iter(names(dt_source), checkFunc = transformable)
  nextEl <- function() {
    col_name <- nextElem(colname_iter)
    transformer_id <- which(tf_names == col_name)
    return(transformer[[transformer_id]])
  }
  obj <- list(nextElem=nextEl)
  class(obj) <- c('iforever','abstractiter','iter')
  obj
}

# Deprecated
# Returns column and iterator, useful for parallel processing but tends to
# encourage breaking the idea of modifying by reference, which is prefered here
# column_and_transformer_iterator <- function(dt_source, transformer) {
#   tf_names <- sapply(transformer, function(x) x$col_name)
#   transformable <- function(x) return(x %in% tf_names)
#   colname_iter <- iter(names(dt_source), checkFunc = transformable)
#   nextEl <- function() {
#     col_name <- nextElem(colname_iter)
#     transformer_id <- which(tf_names == col_name)
#     return(list(
#       col_name = col_name,
#       col = dt_source[[col_name]],
#       transformer = transformer[[transformer_id]]
#     ))
#   }
#   obj <- list(nextElem=nextEl)
#   class(obj) <- c('iforever','abstractiter','iter')
#   obj
# }

apply_transformer_number <- function(col_old, transformer) {
  if("winsor" %in% names(transformer)) {
    transformer$winsor
  }
}

apply_transformer_character <- function() {}
apply_transformer_factor <- function() {}
apply_transformer_logical <- function() {}

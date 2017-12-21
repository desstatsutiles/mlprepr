
require(data.table)
require(iterators)
require(foreach)
require(caret)
require(reshape2)
require(doParallel)

# Learn a transform based on a data.table and its target column ---------------

learn_transformer <- function(dt_source) {
  list_of_transforms <- foreach(col_i = column_iterator(dt_source)) %do% {
    switch(class(col_i),
           "integer" = stop("Not Implemented Yet"),
           stop("Found type that is not supported")
           )
  }
  return(list_of_transforms)
}

column_iterator <- function(dt_source) {

}

learn_transformer_number <- function() {}
learn_transformer_character <- function() {}
learn_transformer_factor <- function() {}
learn_transformer_boolean <- function() {}

# Learn a transform based on a data.table and its target column ---------------

apply_transformer <- function(dt_source) {
  # Compute iterator on columns their corresponding transformer
  iter_ct <- column_and_transformer_iterator(dt_source, list_of_transforms)
  # Use iterator to loop on each column
  list_of_columns <- foreach(col_i = iter_ct) %do% {
    # Compute the column(s) resulting from this transform applied to this col
    NULL
  }
  dt_res <- do.call(cbind, list_of_columns)
}

column_and_transformer_iterator <- function() {

}

apply_transformer_number <- function() {}
apply_transformer_character <- function() {}
apply_transformer_factor <- function() {}
apply_transformer_boolean <- function() {}

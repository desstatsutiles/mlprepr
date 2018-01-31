
require(foreach)
require(data.table)
require(iterators)

#' A function to learn the winsorization of each column
#'
#' This function allows you to learn the winsorization of each column
#' @param dt the table to learn from
#' @param percent.min the quantile used for winsorization, defaults to 5% interval
#' @param percent.max the quantile used for winsorization, defaults to 95% interval
#' @keywords data.table winsor
#' @export
#' @examples
#' WinsorLearn(my_data_table, my_percentage)
WinsorLearn <- function(dt, percent.min = 5/100, percent.max = 95/100) {
  my_list <- foreach(n = 1:ncol(dt)) %do% {
    col.val <- dt[[n]]
    col.nam <- names(dt)[n]
    if(is.numeric(col.val)) {
      return(data.table(name = col.nam,
                        min = quantile(col.val, probs = 0.05),
                        max = quantile(col.val, probs = 0.95)))
    } else {
      return(NULL)
    }
  }
  return(rbindlist(my_list))
}

#' A function to apply an existing winsorization to a data.table
#'
#' This function allows you to apply an existing winsorization to a data.table
#' @param dt the table to winsorize
#' @param winsor_model the existing model
#' @keywords data.table winsor
#' @export
#' @examples
#' WinsorLearn(my_data_table, my_percentage)
WinsorPredict <- function(dt, winsor_model) {
  foreach(n = 1:ncol(dt)) %do% {
    col.val <- dt[[n]]
    col.nam <- names(dt)[n]
    model <- winsor_model[name == col.nam]
    if(nrow(model) == 1) {
      col.newval <- pmin(col.val, model$max, na.rm = T)
      col.newval <- pmax(col.newval, model$min, na.rm = T)
      dt[, (col.nam) := col.newval]
    } else {
      warning(paste0("column '", col.nam, "' ignored in WinsorPredict : no such column in model"))
    }
  }
  return(dt)
}

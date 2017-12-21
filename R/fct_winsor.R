
require(data.table)

#' A function to learn the winsorization of a column
#'
#' This function allows you to learn the winsorization of a column
#' @param col.val the column to winsorize
#' @param percent.min the quantile used for winsorization, defaults to 5%
#' @param percent.max the quantile used for winsorization, defaults to 95%
#' @return a list with two values called "min" and "max"
#' @keywords data.table winsor
#' @export
#' @examples
#' WinsorLearn(col, 0.05, 0.95)
winsor_learn <- function(col.val, percent.min = 5/100, percent.max = 95/100) {
  min = quantile(col.val, probs = percent.min)
  max = quantile(col.val, probs = percent.max)
  return(list(min = min, max = max))
}

#' A function to apply an existing winsorization to a column
#'
#' This function allows you to apply an existing winsorization to a column
#' @param col.val the column to winsorize
#' @param the name of the column to create
#' @param model a list with two values called "min" and "max"
#' @return a data.table with the new column
#' @keywords data.table winsor
#' @export
#' @examples
#' WinsorLearn(col, "mycolname", list(min = 0.05, max = 0.95))
winsor_predict <- function(col.val, col.nam, model) {
  col.newval <- pmin(col.val, model$max, na.rm = T)
  col.newval <- pmax(col.newval, model$min, na.rm = T)
  dt <- data.table(col.newval)
  setnames(dt, col.nam)
  return(dt)
}

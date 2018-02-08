
# A function to learn the winsorization of a column
#
# This function allows you to learn the winsorization of a column
# @param col.val the column to winsorize
# @param percent.min the quantile used for winsorization, defaults to 5%
# @param percent.max the quantile used for winsorization, defaults to 95%
# @return a list with two values called "min" and "max"
# @examples
# WinsorLearn(col, 0.05, 0.95)
winsor_learn <- function(col.val, percent.min = 5/100, percent.max = 95/100) {
  min = quantile(na.omit(col.val), probs = percent.min)
  max = quantile(na.omit(col.val), probs = percent.max)
  res <- list(min = min, max = max)
  mesg = paste0(res, collapse = ", ")
  my_log("winsor_learn (result)", mesg = mesg, type = "message")
  return(res)
}

# A function to apply an existing winsorization to a column
#
# This function allows you to apply an existing winsorization to a column
# @param col.val the column to winsorize
# @param the name of the column to create
# @param min the minimum value allowed. Those which are below are replaced.
# @param max same as min, but above max value.
# @return a data.table with the new column
# @examples
# WinsorLearn(col, "mycolname", list(min = 0.05, max = 0.95))
winsor_predict <- function(col.val, col.nam, min, max) {
  mesg = paste(min, max)
  my_log("winsor_predict (result)", mesg = mesg, type = "message")
  col.newval <- pmin(col.val, max, na.rm = T)
  col.newval <- pmax(col.newval, min, na.rm = T)
  dt <- data.table(col.newval)
  setnames(dt, col.nam)
  return(dt)
}

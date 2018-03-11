
# A function to learn the winsorization of a column
#
# This function allows you to learn the winsorization of a column
# @param col.val the column to winsorize
# @param percent.min the quantile used for winsorization, defaults to 5%
# @param percent.max the quantile used for winsorization, defaults to 95%
# @params winsor_moda_safe number of modas that are safe from winsor
# @return a list with two values called "min" and "max"
# @examples
# WinsorLearn(col, 0.05, 0.95)
winsor_learn <- function(col.val,
                         percent.min = 5/100,
                         percent.max = 95/100,
                         winsor_moda_safe = 10) {
  min = quantile(na.omit(col.val), probs = percent.min)
  max = quantile(na.omit(col.val), probs = percent.max)
  top_k <- sort(table(col.val), decreasing = T)
  top_k <- top_k[top_k >= 2]
  if(length(top_k) > 0) {
    n <- min(length(top_k), winsor_moda_safe)
    top_k <- as.numeric(names(top_k[1:n]))
  } else {
    top_k <- NA
  }
  res <- list(min = min, max = max, top = top_k)
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
# @param top the values that we dont want to winsorize
# @return a data.table with the new column
# @examples
# WinsorLearn(col, "mycolname", list(min = 0.05, max = 0.95))
winsor_predict <- function(col.val, col.nam, min, max, top) {
  mesg = paste(min, max)
  # my_log("winsor_predict (result)", mesg = mesg, type = "message")
  col.newval <- pmin(col.val, max, na.rm = T)
  col.newval <- pmax(col.newval, min, na.rm = T)
  if(!(any(is.na(top)) | length(top) <= 0)) {
    # I replace the values of col.val that are not in "top" by the corresponding
    # col.newval values, that are winsorized. The goal is clearly to only modify
    # the values that are not in top.
    col.val[!col.val %in% top] <- col.newval[!col.val %in% top]
  } else {
    col.val <- col.newval
  }
  dt <- data.table(col.val)
  setnames(dt, col.nam)
  return(dt)
}

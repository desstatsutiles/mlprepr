
#' A wrapper around data.table fread and nothing else
#'
#' This function allows you to load a csv table fast
#' @param source The path to the file to load
#' @param double Should we load twice to make sure the types are ok (def : T)
#' @keywords data.table load fread read
#' @export
#' @examples
#' load_data("data/kaggle_titanic_train.csv", T)
#' @keywords internal
#' LoadData()
load_data <- function(source, double = T, ...) {
  dt <- data.table::fread(source, stringsAsFactors = T, ...)
  if(double) {
    my_print("load_data", "performing double loading (slower, more accurate)")
    dt_types <- sapply(dt, class)
    dt <- data.table::fread(source, colClasses = dt_types, ...)
  }
  if(!HIDE_PRINTS) {
    dt_summary <- data.table(
      col = names(dt),
      class = sapply(dt, class),
      distinct = sapply(dt, function(x) length(unique(x))))
    my_print("load_data", "displaying summary of data :")
    print(dt_summary)
  }
  return(dt)
}

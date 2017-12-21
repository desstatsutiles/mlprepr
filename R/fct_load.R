
#' A wrapper around data.table fread and nothing else
#'
#' This function allows you to load a csv table fast
#' @param source The path to the file to load
#' @keywords data.table load fread read
#' @export
#' @examples
#' LoadData()
load_data <- function(source, ...) {
  dt <- data.table::fread(source, ...)
  return(dt)
}

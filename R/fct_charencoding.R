
require(stringr)

GroupModas <- function(col, min_nb = 1000, others_name = "others") {
  if(!is.factor(col)) col <- as.factor(col)
  nb_per <- sort(table(col), decreasing = T)
  to_others <- nb_per[nb_per < min_nb]
  if(length(to_others) > 0) {
    levels(col)[levels(col) %in% names(to_others)] <- others_name
  }
  return(col)
}

RecodeEmptyString <- function(col, empty_name = "emptystring") {
  if(!is.factor(col)) col <- as.factor(col)
  levels(col)[stringr::str_trim(levels(col)) == ""] <- empty_name
  return(col)
}

# OneHotEncoding <- function(dt, formula = target ~ .) {
#   res <- dummyVars(formula, dt)
# }

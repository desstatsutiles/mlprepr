
# Encoding NAs properly
encode_nas <- function(dt, va_num = -1, va_char = "NA", colname_na = "_NA") {
  setDT(dt)
  dt_names <- copy(names(dt))
  for (j in seq_along(dt)) {
    this_col <- dt[[j]]
    i_na <- as.integer(is.na(this_col))
    any_na = sum(i_na) > 0
    encoded_as = NA
    if(any_na) {
      # Create a column to keep track of NAs
      na_name <- paste0(dt_names[j], colname_na)
      set(dt, j = na_name, value = i_na)
      # Impute a reasonable value
      encoded_as <- na_replacement_by_class(this_col, va_num, va_char)
      data.table::set(dt, i = which(i_na == 1L), j = j, value = encoded_as)
    }
  }
  return(dt)
}

na_replacement_by_class <- function(vec, va_num = -1, va_char = "NA") {
  if (is.factor(vec) | is.character(vec)) {
    encoded_as = va_char
  } else if (is.numeric(vec)) {
    encoded_as = va_num
  } else if (is.logical(vec)) {
    encoded_as = TRUE
  } else {
    stop("encode_nas : unexpected type, encoding as first")
  }
  return(encoded_as)
}





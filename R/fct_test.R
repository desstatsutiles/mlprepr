
require(stringr)
require(foreach)
require(data.table)
require(iterators)

# Test data.tables to check that we're still good

test_dt_1 <- function(n = 1000, dates = T) {
  dt = data.table(int_id = 1:n)
  dt[, int_sample_10 := sample(1:10, n, replace = T)]
  dt[, int_unbalance_10 := unbalance(1:10, n)]
  dt[, num_runif := runif(n)]
  dt[, num_rexp := rexp(n)]
  dt[, letters := sample(LETTERS, n, replace = T)]
  dt[, strings_random := generate_strings(n)]
  dt[, strings_sample_10 := sample(generate_strings(10), n, replace = T)]
  dt[, strings_unbalance_10 := unbalance(generate_strings(10), n)]
  dt[, boolean := sample(c(T, F), n, replace = T)]
  dt[, boolean_rare := sample(c(rep(T, 100), F), n, replace = T)]
  if(dates) {
    dt[, date := sample(generate_dates(100), n, replace = T)]
  }
  return(dt)
}

# https://stackoverflow.com/questions/42734547/generating-random-strings
generate_strings <- function(n = 5000) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

unbalance <- function(my_sample,
                      n = 1000,
                      my_values = small_and_big(length(my_sample))) {
  my_table <- data.table(s = my_sample,
                         v = sample(my_values, length(my_sample), replace = T))
  values <- foreach(val = iter(my_table, by="row")) %do% {
    rep(val$s, val$v)
  }
  return(sample(unlist(values), n, replace = T))
}

small_and_big <- function(n, min = 1, max = 100, rate = 1/4) {
  v1 <- unique(floor(rexp(100, rate = rate)))
  v2 <- 100 - unique(floor(rexp(100, rate = rate)))
  return(sort(unique(c(v1, v2))))
}

generate_date <- function(date = Sys.Date(), format = "%d/%m/%Y") {
  return(format(date - round(exp(runif(1, max = 10))), format))
}

generate_dates <- function(n = 1000) {
  res <- foreach(i = 1:n) %do% {
    generate_date()
  }
  return(unlist(res))
}

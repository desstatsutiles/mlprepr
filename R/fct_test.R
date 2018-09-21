
# Test data.tables to check that we're still good

test_dt_1 <- function(n = 1000,
                      dates = T,
                      seed = NA) {
  if(!is.na(seed)) set.seed(seed)
  dt = data.table(int_id = 1:n)
  set(dt, j = "int_sample_10", value = sample(1:10, n, replace = T))
  set(dt, j = "int_unbalance_10", value = unbalance(1:10, n))
  set(dt, j = "num_runif", value = runif(n))
  set(dt, j = "num_rexp", value = rexp(n))
  set(dt, j = "letters", value = sample(LETTERS, n, replace = T))
  set(dt, j = "letters_fac", value = factor(sample(LETTERS, n, replace = T)))
  set(dt, j = "strings_random", value = generate_strings(n))
  set(dt, j = "strings_random_fac", value = factor(generate_strings(n)))
  set(dt, j = "strings_sample_10", value = sample(generate_strings(10), n, replace = T))
  set(dt, j = "strings_unbalance_10", value = unbalance(generate_strings(10), n))
  set(dt, j = "boolean", value = sample(c(T, F), n, replace = T))
  set(dt, j = "boolean_rare", value = sample(c(rep(T, 100), F), n, replace = T))
  if(dates) {
    set(dt, j = "date", value = sample(generate_dates(100), n, replace = T))
  }
  return(dt)
}

# Same as test_dt_1 but with missing values
test_dt_2 <- function(n = 1000,
                      dates = T,
                      na_chance = 0.8,
                      na_ratio = 0.1,
                      seed = NA) {
  if(!is.na(seed)) set.seed(seed)
  dt <- test_dt_1(n, dates)
  setDT(dt)
  cols <- sample(copy(names(dt)), size = round(ncol(dt) * na_chance))
  foreach(col = cols) %do% {
    i_na <- sample(1:nrow(dt), size = round(nrow(dt) * na_ratio))
    set(dt, i = i_na, j = col, value = NA)
    invisible()
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

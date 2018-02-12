library(mlprepr)
context("Handling NAs")

test_that("NAs on complex dataset", {
  dt <- function() test_dt_2(seed = 42)
  expect_equal(nrow(encode_nas(dt())), nrow(dt()))
  expect_equal(nrow(na.omit(encode_nas(dt()))), nrow(dt()))
})

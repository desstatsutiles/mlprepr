library(mlprepr)
context("Test that tests work")

test_that("testing tests", {
  expect_equal(1, 1)
  expect_equal_to_reference(1, file = "D:/Data/Root/Pro/tmp/test.rds")
})

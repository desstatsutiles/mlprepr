library(mlprepr)
context("Drift only")

test_that("drift on real datasets", {
  # Define iris test ----------------------------------------------------------
  test_iris <- function() {
    dt_iris <- data.table(iris)
    drift_iris <- drift_detector(dt_iris)
    return(drift_iris)
  }
  # Define titanic test -------------------------------------------------------
  test_titanic <- function() {
    source("R/fct_main.R")
    # loading
    dt_train <- load_data("data/kaggle_titanic_train.csv")
    dt_test <- load_data("data/kaggle_titanic_test.csv")
    # learn
    params = learn_transformer_parameters(target_colname = "Survived")
    transformer <- learn_transformer(dt_train, params = params)
    # apply
    apply_transformer(dt_train, transformer)
    apply_transformer(dt_test, transformer)
    # drift
    dt_train[, Survived := NULL]
    drift_titanic <- drift_detector(copy(dt_train), copy(dt_test))
    return(drift_titanic)
  }
  # Actually test -------------------------------------------------------------
  expect_equal_to_reference(drift_detector(test_iris()))
  expect_equal_to_reference(drift_detector(test_titanic()))
})

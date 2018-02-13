library(mlprepr)
context("Drift only")

test_that("drift on real datasets", {
  # Define iris test ----------------------------------------------------------
  test_iris <- function() {
    set.seed(42)
    dt_iris <- data.table(iris)
    drift_iris <- drift_detector(dt_iris)
    drift_result <- drift_print(drift_iris, return_table = T)
    return(drift_result[, c("column", "is_drift"), with = F])
  }
  # Define titanic test -------------------------------------------------------
  test_titanic <- function() {
    set.seed(42)
    # loading
    dt_train <- copy(kaggle_titanic_train)
    dt_test <- copy(kaggle_titanic_test)
    # learn
    params = learn_transformer_parameters(target_colname = "Survived")
    transformer <- learn_transformer(dt_train, params = params)
    # apply
    apply_transformer(dt_train, transformer)
    apply_transformer(dt_test, transformer)
    # drift
    set(dt_train, j = "Survived", value = NULL)
    drift_titanic <- drift_detector(copy(dt_train), copy(dt_test))
    drift_result <- drift_print(drift_titanic, return_table = T)
    return(drift_result[, c("column", "is_drift"), with = F])
  }
  # Actually test -------------------------------------------------------------
  expect_equal_to_reference(test_iris(),
                            file = "D:/Data/Root/Pro/tmp/drift_1.rds")
  expect_equal_to_reference(test_titanic(),
                            file = "D:/Data/Root/Pro/tmp/drift_2.rds")
})

test_that("drift on generated datasets", {
  # Define test for 1 dataset -------------------------------------------------
  test_gen <- function(dt_gen) {
    drift_gen <- drift_detector(dt_gen)
    drift_result <- drift_print(drift_gen, return_table = T)
    return(drift_result[, c("column", "is_drift"), with = F])
  }
  # Define test for 2 datasets ------------------------------------------------
  test_gen_two <- function(func = test_dt_1, seed = 42) {
    # loading
    dt <- func(n = 2000, seed = seed)
    dt_train <- copy(dt[1:1000,])
    dt_test <- copy(dt[1001:2000,])
    # learn
    params = learn_transformer_parameters(target_colname = "int_id")
    transformer <- learn_transformer(dt_train, params = params)
    # apply
    apply_transformer(dt_train, transformer)
    apply_transformer(dt_test, transformer)
    # drift
    set(dt_train, j = "int_id", value = NULL)
    drift_titanic <- drift_detector(copy(dt_train), copy(dt_test))
    drift_result <- drift_print(drift_titanic, return_table = T)
    return(drift_result[, c("column", "is_drift"), with = F])
  }
  # Actually test -------------------------------------------------------------
  expect_equal_to_reference(
    test_gen(test_dt_1(seed = 42)),
    file = "D:/Data/Root/Pro/tmp/drift_dt_1.rds")
  expect_equal_to_reference(
    test_gen(test_dt_2(seed = 42)),
    file = "D:/Data/Root/Pro/tmp/drift_dt_2.rds")
  expect_equal_to_reference(
    test_gen_two(func = mlprepr:::test_dt_1),
    file = "D:/Data/Root/Pro/tmp/drift_two_dt_1.rds")
  expect_equal_to_reference(
    test_gen_two(func = mlprepr:::test_dt_2),
    file = "D:/Data/Root/Pro/tmp/drift_two_dt_2.rds")
})

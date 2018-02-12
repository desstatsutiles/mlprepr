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

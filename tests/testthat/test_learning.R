library(mlprepr)
context("Learn only")

test_that("learning parameters only", {
  expect_equal_to_reference(
    learn_transformer_parameters(),
    file = "D:/Data/Root/Pro/tmp/learning_1.rds")
})

# Real data -------------------------------------------------------------------
test_that("learning on real dataset", {
  test_learn <- function(dt, target) {
    params <- learn_transformer_parameters(target_colname = target)
    transformer <- learn_transformer(dt, params = params)
    return(transformer)
  }
  expect_equal_to_reference(
    test_learn(data.table(iris), "Species"),
    file = "D:/Data/Root/Pro/tmp/learning_2.rds")
  expect_equal_to_reference(
    test_learn(copy(kaggle_titanic_train), "Survived"),
    file = "D:/Data/Root/Pro/tmp/learning_3.rds")
})

# Generated data (with NA) ----------------------------------------------------
test_that("learning on generated data", {
  test_learn <- function(dt, target) {
    params <- learn_transformer_parameters(target_colname = target)
    transformer <- learn_transformer(dt, params = params)
    return(transformer)
  }
  expect_equal_to_reference(
    test_learn(test_dt_1(seed = 42), "int_id"),
    file = "D:/Data/Root/Pro/tmp/learning_dt_1.rds")
  expect_equal_to_reference(
    test_learn(test_dt_2(seed = 42), "int_id"),
    file = "D:/Data/Root/Pro/tmp/learning_dt_2.rds")
})

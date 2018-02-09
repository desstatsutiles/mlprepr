library(mlprepr)
context("Learn only")

test_that("learning parameters only", {
  expect_equal_to_reference(
    learn_transformer_parameters(),
    file = "D:/Data/Root/Pro/tmp/learning_1.rds")
})

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
    test_learn(kaggle_titanic_train, "Survived"),
    file = "D:/Data/Root/Pro/tmp/learning_3.rds")
  expect_equal_to_reference(
    test_learn(kaggle_titanic_test, "Survived"),
    file = "D:/Data/Root/Pro/tmp/learning_4.rds")
})

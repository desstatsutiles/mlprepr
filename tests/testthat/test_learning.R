library(mlprepr)
context("Learn only")

test_that("learning on real dataset", {

  test_learn <- function(dt, target) {
    params <- learn_transformer_parameters(target_colname = target)
    transformer <- learn_transformer(dt, params = params)
    return(transformer)
  }

  expect_equal_to_reference(
    test_learn(data.table(iris), "Species"))
  expect_equal_to_reference(test_learn(
    load_datatable("data/kaggle_titanic_train.csv"), "Survived"))
})

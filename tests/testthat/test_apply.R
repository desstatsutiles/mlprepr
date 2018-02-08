library(mlprepr)
context("Learn and apply")

test_that("learning and applying to real", {

  test_real <- function(dt, target, with_copy = F) {
    params <- learn_transformer_parameters(target_colname = target)
    if(with_copy) {
      transformer <- learn_transformer(copy(dt), params = params)
    } else {
      transformer <- learn_transformer(dt, params = params)
    }
    apply_transformer(dt, transformer)
    return(dt)
  }

  expect_equal_to_reference(test_real(
    data.table(iris), "Species"))
  expect_equal_to_reference(test_real(
    load_datatable("data/kaggle_titanic_train.csv"), "Survived"))
  expect_equal_to_reference(test_real(
    data.table(iris), "Species", with_copy = F))
  expect_equal_to_reference(test_real(
    load_datatable("data/kaggle_titanic_train.csv"),
    "Survived", with_copy = F))
})


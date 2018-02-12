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
    data.table(iris), "Species"),
    file = "D:/Data/Root/Pro/tmp/apply_1.rds")
  expect_equal_to_reference(test_real(
    copy(kaggle_titanic_train), "Survived"),
    file = "D:/Data/Root/Pro/tmp/apply_2.rds")
  expect_equal_to_reference(test_real(
    data.table(iris), "Species", with_copy = F),
    file = "D:/Data/Root/Pro/tmp/apply_3.rds")
  expect_equal_to_reference(test_real(
    copy(kaggle_titanic_train),
    "Survived", with_copy = F),
    file = "D:/Data/Root/Pro/tmp/apply_4.rds")
})

test_that("learning and applying to generated data", {
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
  expect_equal_to_reference(
    test_real(test_dt_1(seed = 42), "int_id"),
    file = "D:/Data/Root/Pro/tmp/apply_dt_1.rds")
  expect_equal_to_reference(
    test_real(test_dt_2(seed = 42), "int_id"),
    file = "D:/Data/Root/Pro/tmp/apply_dt_2.rds")
})

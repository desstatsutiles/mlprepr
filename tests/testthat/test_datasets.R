library(mlprepr)
context("Test that datasets are available")

test_that("testing datasets", {
  expect_true(is.data.frame(kaggle_titanic_train))
  expect_true(is.data.table(kaggle_titanic_train))
  expect_equal_to_reference(kaggle_titanic_train,
                            file = "D:/Data/Root/Pro/tmp/dataset_kttr.rds")
  expect_true(is.data.frame(kaggle_titanic_test))
  expect_true(is.data.table(kaggle_titanic_test))
  expect_equal_to_reference(kaggle_titanic_test,
                            file = "D:/Data/Root/Pro/tmp/dataset_ktte.rds")
})

test_that("testing data generation code", {
  expect_equal_to_reference(test_dt_1(seed = 42),
                            file = "D:/Data/Root/Pro/tmp/dataset_dt_1.rds")
  expect_equal_to_reference(test_dt_2(seed = 42),
                            file = "D:/Data/Root/Pro/tmp/dataset_dt_2.rds")
})

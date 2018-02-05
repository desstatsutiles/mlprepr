
require(data.table)
require(foreach)
require(caret)
# require(pROC)
source("R/fct_utils.R")
source('R/fct_load.R')

# TODO : Maybe the name "drift" is not correct here. We will measure the
# difference in AUC based on each variable. The score (from 0.5 to 1) will
# represent the drift level of the variable.

drift_detector <- function(dt1, dt2 = NULL) {
  # Controls
  if("I_position" %in% names(dt1)) stop("drift_detector_ranking : I_position is a forbidden name, sorry")
  if(!is.data.table(dt1)) stop("drift_detector_ranking : expected a data.table")
  # Creating target variable
  if(is.null(dt2)) {
    dt1[, I_position := .I / .N]
  } else {
    if(!is.data.table(dt2)) stop("drift_detector_ranking : expected a data.table for dt2")
    dt1[, I_position := 0L]
    dt2[, I_position := 1L]
    dt1 <- rbindlist(list(dt1, dt2))
    dt1[, I_position := factor(I_position)]
  }
  # Creating model
  col_iter <- column_iterator(dt_source = dt1, target_colname = "I_position")
  model_list <- foreach(dt_i = col_iter) %do% {
    fitControl <- trainControl(method = "cv",
                               number = 5,
                               preProcOptions = c("center", "scale"))
    myGrid <- expand.grid(mtry = 1,
                          # min.node.size = 5,
                          splitrule = "gini")
    fit <- train(I_position ~ .,
                 data = dt_i,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = myGrid)
    list(type = ifelse(is.null(dt2), "self", "train vs test"),
         model = fit,
         name = names(dt_i)[[1]],
         perf = fit$results)
  }
  return(model_list)
}

test_iris <- function() {
  dt_iris <- data.table(iris)
  drift_iris <- drift_detector(dt_iris)
  print(sapply(drift_iris, function(x) x$perf))
  return(drift_iris)
}

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
  sapply(drift_titanic, function(x) x$perf)
  return(drift_titanic)
}

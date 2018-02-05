
require(data.table)
require(foreach)
require(caret)
# require(pROC)
source("R/fct_utils.R")

# TODO : Maybe the name "drift" is not correct here. We will measure the
# difference in AUC based on each variable. The score (from 0.5 to 1) will
# represent the drift level of the variable.

drift_detector_difference <- function(dt1, dt2) {

}

drift_detector_ranking <- function(dt1) {
  # Controls
  if("I_position" %in% names(dt1)) stop("drift_detector_ranking : I_position is a forbidden name, sorry")
  if(!is.data.table(dt1)) stop("drift_detector_ranking : expected a data.table")
  # Creating target variable
  dt1[, I_position := .I / .N]
  # Creating model
  col_iter <- column_iterator(dt_source = dt1, target_colname = "I_position")
  model_list <- foreach(dt_i = col_iter) %do% {
    fitControl <- trainControl(method = "cv",
                               number = 5,
                               preProcOptions = c("center", "scale"))
    # myGrid <- expand.grid(mtry = 20, splitrule = "gini", min.node.size = 5)
    # fit <- train(I_position ~ .,
    #              data = dt_i,
    #              method = "ranger",
    #              trControl = fitControl,
    #              tuneGrid = myGrid)
    # myGrid <- expand.grid(mtry = 20, splitrule = "gini", min.node.size = 5)
    fit <- train(I_position ~ .,
                 data = dt_i,
                 method = "glm",
                 # tuneGrid = myGrid,
                 trControl = fitControl)
    list(model = fit, name = names(dt_i)[[1]])
  }
  return(model_list)
}

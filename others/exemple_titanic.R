
# Loading data
dt_train <- copy(kaggle_titanic_train)

# Define parameters
params <- learn_transformer_parameters(target_colname = "Survived")

# Learn the transformations needed
transformer <- learn_transformer(dt_train, params = params)

# Apply them
apply_transformer(dt_train, transformer)

# Apply them for test set
dt_test <- copy(kaggle_titanic_test)
apply_transformer(dt_test, transformer)

# Create model
dt_train_classif <- copy(dt_train)
dt_train_classif[, Survived := factor(Survived)]
require(caret)
fitControl <- trainControl(method = "cv", number = 5)
myGrid <- expand.grid(nrounds = 20,
                      max_depth = 2,
                      eta = 0.2,
                      gamma = 1,
                      colsample_bytree = .5,
                      min_child_weight = 10,
                      subsample = 0.7)
fit <- train(Survived ~ .,
             data = dt_train_classif,
             method = "xgbTree",
             trControl = fitControl,
             tuneGrid = myGrid,
             preProcess = c("center", "scale"))

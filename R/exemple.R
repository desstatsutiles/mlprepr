
source("R/fct_main.R")
require(data.table)

# Loading data
my_log("main", "load_data")
dt_train <- load_data("data/kaggle_titanic_train.csv")

# Define parameters
params = learn_transformer_parameters(target_colname = "Survived")

# Learn the transformations needed
my_log("main", "learn_transformer")
transformer <- learn_transformer(dt_train, params = params)

# Apply them
my_log("main", "apply_transformer")
apply_transformer(dt_train, transformer)

# Apply them for test set
my_log("main", "apply_transformer for test set")
dt_test <- load_data("data/kaggle_titanic_test.csv")
apply_transformer(dt_test, transformer)

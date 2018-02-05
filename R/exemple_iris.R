
source("R/fct_main.R")
require(data.table)

# Loading data
my_log("main", "load_data")
dt_train <- data.table(iris)

# Define parameters
params = learn_transformer_parameters(target_colname = "Species")

# Learn the transformations needed
my_log("main", "learn_transformer")
transformer <- learn_transformer(dt_train, params = params)

# Apply them
my_log("main", "apply_transformer")
apply_transformer(dt_train, transformer)

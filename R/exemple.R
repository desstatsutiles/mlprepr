
source("R/fct_main.R")
require(data.table)

source("R/fct_load.R")
my_log("main", "load_data")
dt_source <- load_data("data/kaggle_titanic_train.csv")

# Define parameters
params = learn_transformer_parameters(target_colname = "Survived")

# Learn the transformations needed
my_log("main", "learn_transformer")
transformer <- learn_transformer(dt_source[, .(Sex, Survived)], params = params)

# Apply them
my_log("main", "apply_transformer")
dt <- apply_transformer(dt_source, transformer)

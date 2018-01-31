
source("R/fct_main.R")
require(data.table)
# dt_source <- data.table(iris)
dt_source <- data.table(Titanic)[, .(Class, Survived)]

# Define parameters
# params = learn_transformer_parameters(target_colname = "Species")
params = learn_transformer_parameters(target_colname = "N")

# Learn the transformations needed
my_log("main", "learn_transformer")
transformer <- learn_transformer(dt_source, params = params)

# Apply them
my_log("main", "apply_transformer")
dt <- apply_transformer(dt_source, transformer)

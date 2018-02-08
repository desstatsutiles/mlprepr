
require(mlprepr)

# Loading data
dt_train <- data.table(iris)

# Define parameters
params <- learn_transformer_parameters(target_colname = "Species")

# Learn the transformations needed
transformer <- learn_transformer(dt_train, params = params)

# Apply them
dt_train <- data.table(iris)
apply_transformer(dt_train[, .(Petal.Width, Species)], transformer)


# Loading data
dt_train <- load_datatable("data/kaggle_titanic_train.csv")

# Define parameters
params <- learn_transformer_parameters(target_colname = "Survived")

# Learn the transformations needed
transformer <- learn_transformer(dt_train, params = params)

# Apply them
apply_transformer(dt_train, transformer)

# Apply them for test set
dt_test <- load_datatable("data/kaggle_titanic_test.csv")
apply_transformer(dt_test, transformer)

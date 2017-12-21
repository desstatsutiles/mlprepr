
require(data.table)
dt_titanic <- data.table(Titanic)

# Learn the transformations needed
dt_transformer <- learn_transformer(dt_titanic)

# Apply them
dt <- apply_transformer(dt_titanic, dt_transformer)

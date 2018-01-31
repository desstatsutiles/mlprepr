
require(data.table)
require(caret)
require(reshape2)
require(iterators)
require(foreach)

data(iris)
dt <- data.table(iris)
setnames(dt, "Species", "target")
dt[, target := factor(target)]

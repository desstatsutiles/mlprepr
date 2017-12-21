
require(data.table)
DT <- data.table(Titanic)

require(dataPreparation)
DTc <- dataPreparation::prepareSet(copy(DT))

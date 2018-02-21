# Loading data ----------------------------------------------------------------
require(mlprepr)
dt_1 <- kaggle_titanic_train[, -"Survived"]
dt_2 <- kaggle_titanic_test

# Computing drift -------------------------------------------------------------
my_drift_em <- drift_detector(dt_1, dt_2, method = "earthmover")
my_drift_em <- drift_print(my_drift_em, return_table = T)
setnames(my_drift_em, c("column", "em", "em_ori", "em_nok", "ok"))
my_drift_em[, ok := NULL]

my_drift_kl <- drift_detector(dt_1, dt_2, method = "kldivergence")
my_drift_kl <- drift_print(my_drift_kl, return_table = T)
setnames(my_drift_kl, c("column", "kl", "kl1", "kl2", "kl_nok", "ok"))
my_drift_kl[, ok := NULL]

my_drift_xg <- drift_detector(dt_1, dt_2, method = "xgbtree")
my_drift_xg <- drift_print(my_drift_xg, return_table = T)
setnames(my_drift_xg, c("column", "Kappa", "xg_nok", "ok"))
my_drift_xg[, ok := NULL]
set(my_drift_xg, j = "column", value = as.character(my_drift_xg$column))
set(my_drift_xg, j = "Kappa",  value = as.character(my_drift_xg$Kappa))

# Merging drift ---------------------------------------------------------------
md <- data.table:::merge.data.table
my_drift <- md(my_drift_em, my_drift_kl, by = "column")
my_drift <- md(my_drift, my_drift_xg, by = "column")
my_drift <- my_drift[, .(column, em, kl, Kappa, em_nok, kl_nok, xg_nok)]
setkey(my_drift, em, kl, Kappa)
print(my_drift)

# Displaying each column ------------------------------------------------------
dt_both <- rbindlist(list(dt_1, dt_2))
dt_both[, trte := c(rep("train", nrow(dt_1)), rep("test", nrow(dt_2)))]

drift_display_variable(dt = dt_both, 
                       varname = "PassengerId", 
                       categoryname = "trte")

drift_display_variable(dt = dt_both, 
                       varname = "Age", 
                       categoryname = "trte")

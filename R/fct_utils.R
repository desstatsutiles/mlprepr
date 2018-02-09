
# Global parameters -----------------------------------------------------------
.onLoad <- function(libname, pkgname) {

  # Set new options
  op <- options()
  op.mlprepr <- list(
    mlprepr.debug_prints = F,
    mlprepr.debug_logs = F,
    mlprepr.default_Kappa_max = 0.5,
    mlprepr.default_RMSE_min  = 0.2,
    mlprepr.logfile = data.table::data.table(
      time = character(0),
      type = character(0),
      ctxt = character(0),
      mesg = character(0))
  )
  toset <- !(names(op.mlprepr) %in% names(op))
  if(any(toset)) options(op.mlprepr[toset])

  invisible()
}

# Utils functions -------------------------------------------------------------

print_log <- function() getOption("mlprepr.logfile")

log_set <- function(newlog) {
  options(list(mlprepr.logfile = newlog))
  invisible()
}

my_log <- function(ctxt, mesg, type = "message", time = NA,
                   silent = getOption("mlprepr.debug_prints")) {
  if(!silent) {
    if(is.na(time)) time <- Sys.time()
    new_row <- data.table(
      type = as.character(time),
      type = as.character(type),
      ctxt = as.character(ctxt),
      mesg = as.character(mesg)
    )
    old_log <- getOption("mlprepr.logfile")
    new_log <- rbindlist(list(old_log, new_row))
    log_set(new_log)
  }
  invisible()
}

my_log_reset <- function() {
  log_set(data.table(
    time = character(0),
    type = character(0),
    ctxt = character(0),
    mesg = character(0)))
}

my_print <- function(ctxt, mesg, silent = getOption("mlprepr.debug_prints")) {
  my_time <- Sys.time()
  my_log(ctxt, mesg, "message", my_time)
  my_mesg <- paste(my_time, ctxt, ":", mesg, "...")
  if(!silent) {
    message(my_mesg)
  }
}

# dt <- tmp(); dt2 <- tmp(); setnames(dt2, LETTERS[1:4])
# dt2[, V2 := 1:4]
# cbind_by_reference(dt, dt2)
cbind_by_reference <- function(dt, dt2, allow.substitute = T) {
  if(is.null(dt2)) {
    my_log(ctxt = "cbind_by_reference", mesg = "dt2 is null")
    warning(paste("cbind_by_reference : data.table dt2 is null, ignoring this one."))
    return(NULL)
  }
  if(!(is.data.table(dt) & is.data.table(dt2))) {
    my_log(ctxt = "cbind_by_reference", mesg = "dt1 and/or dt2 are/is not data.table(s)")
    warning(paste("cbind_by_reference : expected data.table, ignoring this one."))
    return(NULL)
  }
  is_common_name <- names(dt2) %in% names(dt)
  if(any(is_common_name)) {
    if(allow.substitute) {
      my_print(ctxt = "cbind_by_reference",
               mesg = paste("replacing",
                            paste(names(dt2)[is_common_name],
                                  collapse = ", ")))
    } else {
      stop("Trying to insert existing column")
    }
  }
  new_cols_names <- names(dt2)

  # This syntax looks good but triggers error : Invalid .internal.selfref
  # detected and fixed by taking a (shallow) copy of the data.table
  # dt[, (new_cols_names) := dt2]

  for(ncn in new_cols_names) {
    set(x = dt, i = NULL, j = ncn, value = dt2[[ncn]])
  }
  return(dt)
}

# Iterate on column + target in a data.table
column_iterator <- function(dt_source, target_colname = "target") {
  not_target <- function(x) return(x != target_colname)
  colname_iter <- iter(copy(names(dt_source)), checkFunc = not_target)
  nextEl <- function() {
    next_colname <- nextElem(colname_iter)
    next_cols <- c(next_colname, target_colname)
    my_log("column_iterator (iter)", mesg = next_colname, type = "message")
    return(dt_source[, (next_cols), with=F])
  }
  obj <- list(nextElem=nextEl)
  class(obj) <- c('iforever','abstractiter','iter')
  obj
}

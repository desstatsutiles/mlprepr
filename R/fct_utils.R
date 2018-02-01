
require(data.table)

# Global parameters -----------------------------------------------------------
HIDE_PRINTS <- F
HIDE_LOGS <- F

# Global log table ------------------------------------------------------------
log <- data.table(
  time = character(0),
  type = character(0),
  ctxt = character(0),
  mesg = character(0))

# Utils functions -------------------------------------------------------------

my_log <- function(ctxt, mesg, type = "message", time = NA,
                   silent = HIDE_LOGS) {
  if(is.na(time)) time <- Sys.time()
  new_row <- data.table(
    type = as.character(time),
    type = as.character(type),
    ctxt = as.character(ctxt),
    mesg = as.character(mesg)
  )
  log <<- rbindlist(list(log, new_row))
}

my_log_reset <- function() {
  log <<- data.table(
    time = character(0),
    type = character(0),
    ctxt = character(0),
    mesg = character(0))
}

my_print <- function(ctxt, mesg, silent = HIDE_PRINTS) {
  if(!silent) {
    my_time <- Sys.time()
    my_log(ctxt, mesg, "message", my_time)
    my_mesg <- paste(my_time, ctxt, ":", mesg, "...")
    message(my_mesg)
  }
}

# dt <- tmp(); dt2 <- tmp(); setnames(dt2, LETTERS[1:4])
# dt2[, V2 := 1:4]
# cbind_by_reference(dt, dt2)
cbind_by_reference <- function(dt, dt2, allow.substitute = T) {
  if(any(names(dt2) %in% names(dt))) {
    if(allow.substitute) {
        my_log(ctxt = "cbind_by_reference",
               mesg = paste("replacing", ci),
               type = "message")
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

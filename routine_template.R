
ROUTINE_NAME <- function(ARGS,
                         traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered ROUTINE_NAME\n")
  }

  if (traceThisRoutine) {
    # cat(file = stderr(), myPrepend, "\n")    
  }
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving ROUTINE_NAME\n")
  }
}
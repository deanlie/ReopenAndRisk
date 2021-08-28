
ROUTINE_NAME <- function(theSpecs, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered ROUTINE_NAME\n")
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving ROUTINE_NAME\n")
  }
}
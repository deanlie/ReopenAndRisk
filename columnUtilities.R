
nColumnsBeforeDates <- function(aTibble) {
  theNames <- names(aTibble)
  nCols <- length(theNames)
  warnOption = getOption("warn")
  options(warn = -1)
  nBeforeDates <- 0
  while((nBeforeDates < nCols) && is.na(mdy(theNames[nBeforeDates + 1]))) {
    nBeforeDates <- nBeforeDates + 1
  }
  options(warn = warnOption)
  nBeforeDates
}

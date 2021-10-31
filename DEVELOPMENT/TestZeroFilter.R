keepRowsWithoutZero <- function(aTibble) {
  zeroFreeData <- aTibble %>%
    select(Combined_Key, matches("^[0-9]+"))

  theColNames <- names(zeroFreeData)
  for (i in 2:dim(zeroFreeData)[2]) {
    theColName <- theColNames[i]
    cat(file = stderr(), "theColName =", theColName, "\n")
    zeroFreeData <- zeroFreeData %>%
      filter(.data[[theColName]] > 0)
  }

  return(zeroFreeData)
}

keepRowsWithZero <- function(aTibble) {
  zeroFreeDataSoFar <- aTibble %>%
    select(Combined_Key, matches("^[0-9]+"))
  
  theColNames <- names(zeroFreeData)
  for (i in 2:dim(zeroFreeData)[2]) {
    theColName <- theColNames[i]
    cat(file = stderr(), "theColName =", theColName, "\n")
    zeroContainingData <- zeroFreeDataSoFar %>%
      filter(.data[[theColName]] == 0)
    zeroFreeDataSoFar <- zeroFreeDataSoFar %>%
      filter(.data[[theColName]] > 0)
    if (!is.null(zeroContainingData)) {
      if (!is.null(zeroContainingDataSoFar)) {
        bind_rows(zeroContainingDataSoFar, zeroContainingData)
      } else {
        zeroContainingDataSoFar <- zeroContainingData
      }
    }
  }
  
  return(zeroContainingData)
}

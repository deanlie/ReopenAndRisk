library(tidyverse)

discardTooNewDataFromAFile <- function(thePath,
                                       theTypes,
                                       firstDateToDelete,
                                       traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooNewDataFromFile\n")
  }

  originalData <- read_csv(thePath, col_types = theTypes)
  
  dateColMatch <- as.vector(str_match(names(originalData), "^[1-9]/[1-3]?[0-9]/21$"))
  charNames <- names(originalData)[is.na(dateColMatch)]
  dateNames <- names(originalData)[!is.na(dateColMatch)]

  dataCols <- originalData %>%
    select(any_of(dateNames))

  newNames <- charNames
  for (aName in dateNames) {
    if (mdy(aName) < firstDateToDelete) {
      newNames <- c(newNames, aName)
    }
  }
    
  truncatedTibble <- originalData %>%
      select(any_of({newNames}))
  
  write_csv(truncatedTibble, thePath)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardTooNewDataFromFile\n")
  }

  return(list(T0 = originalData, T1 = truncatedTibble))
}

testDiscardTooNew <- function(traceThisRoutine = FALSE) {
  foo1 <- discardTooNewDataFromAFile("./DATA/US_Vaccinations.csv",
                                     cols(.default = col_double(),
                                          Combined_Key = col_character(),
                                          Datum = col_character(),
                                          Loc_Datum = col_character()), 
                                     Sys.Date(),
                                     traceThisRoutine = traceThisRoutine, prepend = "")
  View(foo1$T0)
  View(foo1$T1)

  return(foo1)
}

library(tidyverse)
library(lubridate)

discardTooNewDataFromAFile <- function(thePath,
                                       theTypes,
                                       firstDateToDelete,
                                       traceThisRoutine = FALSE, prepend = "",
                                       theFileName = "UNKNOWN") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooNewDataFromFile", theFileName, "\n")
  }

  originalData <- read_csv(thePath, col_types = theTypes)
  
  dateColMatch <- as.vector(str_match(names(originalData), "^[1-9]/[1-3]?[0-9]/2?0?21$"))
  charNames <- names(originalData)[is.na(dateColMatch)]
  dateNames <- names(originalData)[!is.na(dateColMatch)]
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Number of charNames", length(charNames), "\n")
    cat(file = stderr(), myPrepend, "Number of dateNames", length(dateNames), "\n")
  }

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

testDiscardTooNew_US_vacc <- function(traceThisRoutine = FALSE) {
  foo1 <- discardTooNewDataFromAFile("./DATA/US_Vaccinations.csv",
                                     cols(.default = col_double(),
                                          Combined_Key = col_character(),
                                          Datum = col_character(),
                                          Loc_Datum = col_character()), 
                                     mdy("08-13-2021"),
                                     traceThisRoutine = traceThisRoutine, prepend = "TEST",
                                     theFileName = "US_Vaccinations.csv")
  return(foo1)
}

testDiscardTooNew_US_I_R <- function(traceThisRoutine = FALSE) {
  foo1 <- discardTooNewDataFromAFile("./DATA/US_Incident_Rate.csv",
                                     cols(.default = col_double(),
                                          Province_State = col_character(),
                                          Combined_Key = col_character()), 
                                     mdy("07-15-2021"),
                                     traceThisRoutine = traceThisRoutine, prepend = "TEST",
                                     theFileName = "US_Incident_Rate.csv")
  return(foo1)
}

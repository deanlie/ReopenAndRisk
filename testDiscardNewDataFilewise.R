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

  # We are expecting mdy parse failures, don't tell us about them.
  warnOption <- getOption("warn")
  options(warn = -1) 
  dateColMatch <- as.vector(mdy(names(originalData)))
  options(warn = warnOption)
  # OK, now report warnings as before

  charNames <- names(originalData)[is.na(dateColMatch)]
  dateNames <- names(originalData)[!is.na(dateColMatch)]

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Number of Columns", length(names(originalData)), "\n")
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


discardDataOutsideDateRangeFromAFile <- function(thePath,
                                                 theTypes,
                                                 firstDateToKeep,
                                                 lastDateToKeep,
                                                 traceThisRoutine = FALSE, prepend = "",
                                                 theFileName = "UNKNOWN") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardDataOutsideDateRangeFromFile", theFileName, "\n")
  }
  
  originalData <- read_csv(thePath, col_types = theTypes)
  
  # We are expecting mdy parse failures, don't tell us about them.
  warnOption <- getOption("warn")
  options(warn = -1) 
  dateColMatch <- as.vector(mdy(names(originalData)))
  options(warn = warnOption)
  # OK, now report warnings as before
  
  charNames <- names(originalData)[is.na(dateColMatch)]
  dateNames <- names(originalData)[!is.na(dateColMatch)]
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Number of Columns", length(names(originalData)), "\n")
    cat(file = stderr(), myPrepend, "Number of charNames", length(charNames), "\n")
    cat(file = stderr(), myPrepend, "Number of dateNames", length(dateNames), "\n")
  }
  
  dataCols <- originalData %>%
    select(any_of(dateNames))
  
  newNames <- charNames
  for (aName in dateNames) {
    aDate <- mdy(aName)
    if ((is.null(firstDateToKeep) | (aDate >= firstDateToKeep)) &
      (is.null(lastDateToKeep) | (aDate <= lastDateToKeep))) {
      newNames <- c(newNames, aName)
    }
  }
  
  truncatedTibble <- originalData %>%
    select(any_of({newNames}))
  
  write_csv(truncatedTibble, thePath)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardDataOutsideDateRangeFromFile\n")
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

testDiscardDataOutsideDateRange_US_vacc <- function(traceThisRoutine = FALSE) {
  foo1 <- discardDataOutsideDateRangeFromAFile("./DATA/US_Vaccinations.csv",
                                     cols(.default = col_double(),
                                          Combined_Key = col_character(),
                                          Datum = col_character(),
                                          Loc_Datum = col_character()),
                                     mdy("06-01-2021"),
                                     mdy("08-12-2021"),
                                     traceThisRoutine = traceThisRoutine, prepend = "TEST_V  ",
                                     theFileName = "US_Vaccinations.csv")
  return(foo1)
}

testDiscardDataOutsideDateRange_US_I_R <- function(traceThisRoutine = FALSE) {
  foo1 <- discardDataOutsideDateRangeFromAFile("./DATA/US_Incident_Rate.csv",
                                     cols(.default = col_double(),
                                          Province_State = col_character(),
                                          Combined_Key = col_character()),
                                     mdy("06-03-2021"),
                                     mdy("07-14-2021"),
                                     traceThisRoutine = traceThisRoutine, prepend = "TEST_I_R",
                                     theFileName = "US_Incident_Rate.csv")
  return(foo1)
}

testSuite <- function() {
  foo1 <- testDiscardDataOutsideDateRange_US_vacc(traceThisRoutine = traceThisRoutine)
}

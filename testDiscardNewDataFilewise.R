library(tidyverse)
library(lubridate)

discardTooNewDataFromAFile <- function(thePath,
                                       theTypes,
                                       firstDateToDelete,
                                       traceThisRoutine = FALSE, prepend = "",
                                       theFileName = "UNKNOWN") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooNewDataFromAFile", theFileName, "\n")
  }

  return_me <- discardDataOutsideDateRangeFromAFile(thePath,
                                              theTypes,
                                              0,
                                              firstDateToDelete - 1,
                                              traceThisRoutine, myPrepend,
                                              theFileName = theFileName)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardTooNewDataFromAFile", theFileName, "\n")
  }
  
  return(return_me)
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
    if (((firstDateToKeep == 0) | (aDate >= firstDateToKeep)) &
        ((lastDateToKeep > Sys.Date()) | (aDate <= lastDateToKeep))) {
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

reinitTestDataFile <- function(traceThisRoutine = FALSE, prepend = "",
                               theFileName = "UNKNOWN") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered reinitTestDataFile", theFileName, "\n")
  }
  
  system(paste("cp DATA/CACHE/", theFileName, " DATA/", sep = ""))
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving reinitTestDataFile", theFileName, "\n")
  }
}

reinitVaccTestData <- function(traceThisRoutine = FALSE, prepend = "",
                               theFileName = "UNKNOWN") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered reinitVaccTestData", theFileName, "\n")
  }
  reinitTestDataFile(traceThisRoutine = traceThisRoutine, prepend = myPrepend,
                     theFileName = "US_Vaccinations.csv")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving reinitVaccTestData", theFileName, "\n")
  }
  # system("cp DATA/CACHE/US_Vaccinations.csv DATA/")
}

reinitIRTestData <- function(traceThisRoutine = FALSE, prepend = "",
                             theFileName = "UNKNOWN") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered reinitIRTestData", theFileName, "\n")
  }
  reinitTestDataFile(traceThisRoutine = traceThisRoutine, prepend = myPrepend,
                     theFileName = "US_Incident_Rate.csv")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving reinitIRTestData", theFileName, "\n")
  }
  #  system("cp DATA/CACHE/US_Incident_Rate.csv DATA/")
}

testDiscardTooNew_US_vacc <- function(traceThisRoutine = FALSE, prepend = "",
                                      theFileName = "UNKNOWN") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testDiscardTooNew_US_vacc", theFileName, "\n")
  }
  
  reinitVaccTestData(traceThisRoutine = traceThisRoutine, prepend = myPrepend,
                     theFileName = "US_Vaccinations.csv")
  foo1 <- discardTooNewDataFromAFile("./DATA/US_Vaccinations.csv",
                                     cols(.default = col_double(),
                                          Combined_Key = col_character(),
                                          Datum = col_character(),
                                          Loc_Datum = col_character()), 
                                     mdy("08-13-2021"),
                                     traceThisRoutine = traceThisRoutine,
                                     prepend = myPrepend,
                                     theFileName = "US_Vaccinations.csv")

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testDiscardTooNew_US_vacc", theFileName, "\n")
  }
  
  return(foo1)
}

testDiscardTooNew_US_I_R <- function(traceThisRoutine = FALSE, prepend = "",
                                     theFileName = "UNKNOWN") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testDiscardTooNew_US_I_R", theFileName, "\n")
  }
  
  reinitIRTestData(traceThisRoutine = traceThisRoutine, prepend = myPrepend,
                   theFileName = theFileName)
  foo1 <- discardTooNewDataFromAFile(paste("./DATA/", theFileName, sep = ""),
                                     cols(.default = col_double(),
                                          Province_State = col_character(),
                                          Combined_Key = col_character()), 
                                     mdy("07-15-2021"),
                                     traceThisRoutine = traceThisRoutine, prepend = myPrepend,
                                     theFileName = theFileName)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testDiscardTooNew_US_I_R", theFileName, "\n")
  }
  
  return(foo1)
}

testDiscardDataOutsideDateRange_US_vacc <- function(traceThisRoutine = FALSE, prepend = "",
                                                    theFileName = "UNKNOWN") {
  myPrepend <- paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testDiscardDataOutsideDateRange_US_vacc", theFileName, "\n")
  }
  
  reinitVaccTestData(traceThisRoutine = traceThisRoutine, prepend = myPrepend,
                     theFileName = theFileName)
  foo1 <- discardDataOutsideDateRangeFromAFile(paste("./DATA/", theFileName, sep = ""),
                                     cols(.default = col_double(),
                                          Combined_Key = col_character(),
                                          Datum = col_character(),
                                          Loc_Datum = col_character()),
                                     mdy("06-01-2021"),
                                     mdy("08-12-2021"),
                                     traceThisRoutine = traceThisRoutine, prepend = myPrepend,
                                     theFileName = theFileName)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testDiscardDataOutsideDateRange_US_vacc", theFileName, "\n")
  }
  
  return(foo1)
}

testDiscardDataOutsideDateRange_US_I_R <- function(traceThisRoutine = FALSE, prepend = "",
                                                   theFileName = "UNKNOWN") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testDiscardDataOutsideDateRange_US_I_R", theFileName, "\n")
  }
  
  reinitIRTestData(traceThisRoutine = traceThisRoutine, prepend = myPrepend,
                   theFileName = theFileName)
  foo1 <- discardDataOutsideDateRangeFromAFile(paste("./DATA/", theFileName, sep = ""),
                                     cols(.default = col_double(),
                                          Province_State = col_character(),
                                          Combined_Key = col_character()),
                                     mdy("06-03-2021"),
                                     mdy("07-14-2021"),
                                     traceThisRoutine = traceThisRoutine, prepend = myPrepend,
                                     theFileName = theFileName)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testDiscardDataOutsideDateRange_US_I_R", theFileName, "\n")
  }
  
  return(foo1)
}

testSuite <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testSuite\n")
  }
  foo1 <- testDiscardDataOutsideDateRange_US_vacc(traceThisRoutine = traceThisRoutine,
                                                  prepend = myPrepend,
                                                  theFileName = "US_Vaccinations.csv")
  foo2 <- testDiscardTooNew_US_vacc(traceThisRoutine = traceThisRoutine,
                                    prepend = myPrepend,
                                    theFileName = "US_Vaccinations.csv")
  foo4 <- testDiscardDataOutsideDateRange_US_I_R(traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend,
                                                 theFileName = "US_Incident_Rate.csv")
  foo5 <- testDiscardTooNew_US_I_R(traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend,
                                   theFileName = "US_Incident_Rate.csv")
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testSuite\n")
  }

  return(list(ORIG_123 = foo1$T0
              , RES1 = foo1$T1
              , RES2 = foo2$T1
              , ORIG_456 = foo4$T0
              , RES4 = foo4$T1
              , RES5 = foo5$T1
              ))
}

library(tidyverse)
library(lubridate)

discardDataOutsideDateRangeFromATibble <- function(originalData,
                                                   firstDateToKeep,
                                                   lastDateToKeep,
                                                   traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardDataOutsideDateRangeFromATibble\n")
  }

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
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Number of columns to retain", length(newNames), "\n")
    cat(file = stderr(), myPrepend, "Retaining cols",
        newNames[4], "...",
        newNames[length(newNames)], "\n")
  }
  
  truncatedTibble <- originalData %>%
    select(any_of({newNames}))
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardDataOutsideDateRangeFromATibble\n")
  }
  
  return(truncatedTibble)
}

discardDataOutsideDateRangeFromAFile <- function(thePath,
                                                 firstDateToKeep,
                                                 lastDateToKeep,
                                                 traceThisRoutine = FALSE,
                                                 prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardDataOutsideDateRangeFrom", thePath, "\n")
  }
  
  originalData <- read_csv(thePath, show_col_types = FALSE)
  
  truncatedData <- discardDataOutsideDateRangeFromATibble(originalData,
                                                          firstDateToKeep,
                                                          lastDateToKeep,
                                                          traceThisRoutine = traceThisRoutine,
                                                          prepend = myPrepend)
  
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

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Number of columns to retain", length(newNames), "\n")
    cat(file = stderr(), myPrepend, "Retaining cols",
        newNames[4], "...",
        newNames[length(newNames)], "\n")
  }
  
  truncatedTibble <- originalData %>%
    select(any_of({newNames}))
  
  write_csv(truncatedTibble, thePath)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardDataOutsideDateRangeFromFile\n")
  }
  
  return(list(T0 = originalData, T1 = truncatedTibble))
}

# Convenience routines for the above
discardTooNewDataFromAFile <- function(thePath,
                                       firstDateToDelete,
                                       traceThisRoutine = FALSE,
                                       prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooNewDataFromA", thePath, "\n")
  }
  
  return_me <- discardDataOutsideDateRangeFromAFile(thePath,
                                                    0,
                                                    firstDateToDelete - 1,
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardTooNewDataFromAFile", thePath, "\n")
  }
  
  return(return_me)
}

discardTooOldDataFromAFile <- function(thePath,
                                       firstDateToKeep,
                                       traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooOldDataFromAFile", thePath, "\n")
  }
  
  return_me <- discardDataOutsideDateRangeFromAFile(thePath,
                                                    firstDateToKeep,
                                                    Sys.Date() + 2,
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardTooOldDataFromAFile", thePath, "\n")
  }
  
  return(return_me)
}

discardPopCol <- function(sourceTibble) {
  colNames <- names(sourceTibble)
  if ("Population" %in% colNames) {
    cat(file = stderr(), "    removing 'Population' column\n")
    resultTibble <-  sourceTibble %>%
      select(!Population)
  } else {
    resultTibble <- sourceTibble
  }
  return(resultTibble)
}

discardDotY <- function(sourceTibble) {
  theMatches <- str_match(names(sourceTibble), ".*\\.y")
  goodMatches <- theMatches[!is.na(theMatches)]
  lastState <- sourceTibble
  for (aGoodMatch in goodMatches) {
    processThis <- lastState
    cat(file = stderr(), "    removing col", aGoodMatch, "\n")
    lastState <- select(processThis, -{{aGoodMatch}})
  }
  return(lastState)
}

renameDot1016x <- function(sourceTibble) {
  theNames <- names(sourceTibble)
  for (i in 1:length(theNames)) {
    if (theNames[i] == "10/16/21.x") {
      cat(file = stderr(), "    renaming 10/16/21.x\n")
      names(sourceTibble)[i] <- "10/16/21"
      break
    }
  }
  return(sourceTibble)
}

clipDates <- function(sourceTibble) {
  resultTibble <- sourceTibble %>%
    discardDataOutsideDateRangeFromATibble(mdy("10/25/2021"),
                                           mdy("12/07/2021"),
                                           traceThisRoutine = FALSE, prepend = "")
}

processTibble <- function(sourceTibble) {
  resultTibble <- clipDates(sourceTibble)
}

zapFiles <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered zapFiles\n")
  }
  
  # for (aName in c("US_Case_Fatality_Ratio.csv",
  #                 "US_Confirmed.csv",
  #                 "US_County_Confirmed.csv",
  #                 "US_Incident_Rate.csv",
  #                 "US_State_Case_Fatality_Ratio.csv",
  #                 "US_County_Deaths.csv",
  #                 "US_Deaths.csv",
  #                 "US_State_Confirmed.csv",
  #                 "US_State_Deaths.csv",
  #                 "US_Incident_Rate.csv",
  #                 "US_State_Case_Fatality_Ratio.csv",
  #                 "US_State_Incident_Rate.csv",
  #                 "US_State_Population_Est.csv",
  #                 "US_State_Testing_Rate.csv",
  #                 "US_State_Total_Test_Results.csv",
  #                 "US_State_Vaccinations.csv",
  #                 "US_Testing_Rate.csv",
  #                 "US_Total_Test_Results.csv",
  #                 "US_Vaccinations.csv")) {
  for (aName in c("US_State_Vaccinations.csv",
                  "US_Vaccinations.csv")) {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Processing", aName, "\n")
    }

    sourceTibble <- read_csv(paste(testSourceDir(),
                                   aName,
                                   sep = ""),
                             show_col_types = FALSE)
    
    newTibble <- processTibble(sourceTibble)
    
    write_csv(newTibble, paste(testDestDir(), aName, sep = ""))
    
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Wrote modified", aName, "\n")
    }
  }
  
  if (traceThisRoutine) {
    # cat(file = stderr(), myPrepend, "\n")    
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving zapFiles\n")
  }
}

clipOneVariousEnd <- function(theFileName, lastDateToKeep) {
  theDirectory <- "./DATA/STATIC/VariousEnds/"

  discardDataOutsideDateRangeFromAFile(paste(theDirectory,
                                             theFileName,
                                             ".csv",
                                             sep = ""),
                                       as.Date("2021-09-20"),
                                       lastDateToKeep,
                                       traceThisRoutine = TRUE,
                                       prepend = "")
}

clipSeveralEnds <- function() {
  clipOneVariousEnd("US_County_Confirmed", as.Date("2021-11-17"))
  clipOneVariousEnd("US_Deaths", as.Date("2021-11-17"))
  clipOneVariousEnd("US_State_Vaccinations", as.Date("2021-11-17"))
  clipOneVariousEnd("US_County_Deaths", as.Date("2021-11-16"))
  clipOneVariousEnd("US_Vaccinations", as.Date("2021-11-16"))
  clipOneVariousEnd("US_Total_Test_Results", as.Date("2021-11-18"))
}

#####################################################
# For FillVaccDates:
# source downloadJHUData.R
# Read in the big data file, Vacc_TS_URL():
# raw_data <- getURLOrStop(Vacc_TS_URL(), col_types = cols(.default = col_guess()))
#####################################################

#####################################################
# TESTS below this point
#####################################################
testSourceDir <- function() {
  "./DATA/STATIC/"
}

testDestDir <- function() {
  "./DATA/STATIC/"
}

reinitTestDataFile <- function(theFileName,
                               traceThisRoutine = FALSE,
                               prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered reinitTestDataFile\n")
    cat(file = stderr(), myPrepend, "theFileName =", theFileName, "\n")
  }
  
  system(paste("cp ", testSourceDir(), theFileName, testDestDir(), sep = ""))
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving reinitTestDataFile\n")
  }
}

testDiscardTooNew <- function(theFileName,
                              theTypes,
                              firstDateToDelete,
                              traceThisRoutine = FALSE,
                              prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testDiscardTooNew", theFileName, "\n")
  }
  
  reinitTestDataFile(theFileName,
                     traceThisRoutine = traceThisRoutine,
                     prepend = myPrepend)
  
  foo1 <- discardTooNewDataFromAFile(paste(testDestDir(), theFileName, sep=""),
                                     firstDateToDelete,
                                     traceThisRoutine = traceThisRoutine,
                                     prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testDiscardTooNew", theFileName, "\n")
  }
  
  return(foo1)
}

testDiscardTooOld <- function(theFileName,
                              theTypes,
                              firstDateToKeep,
                              traceThisRoutine = FALSE,
                              prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testDiscardTooOld", theFileName, "\n")
  }
  
  reinitTestDataFile(theFileName,
                     traceThisRoutine = traceThisRoutine,
                     prepend = myPrepend)
  
  foo1 <- discardTooOldDataFromAFile(paste(testDestDir(), theFileName, sep=""),
                                     firstDateToKeep,
                                     traceThisRoutine = traceThisRoutine,
                                     prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testDiscardTooOld", theFileName, "\n")
  }
  
  return(foo1)
}

testDiscardDataOutsideDateRange <- function(theFileName,
                                            theTypes,
                                            firstDateToKeep,
                                            lastDateToKeep,
                                            traceThisRoutine = FALSE,
                                            prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testDiscardDataOutsideDateRange_US_vacc", theFileName, "\n")
  }
  
  reinitTestDataFile(theFileName,
                     traceThisRoutine = traceThisRoutine,
                     prepend = myPrepend)
  
  foo1 <- discardDataOutsideDateRangeFromAFile(paste(testDestDir(),
                                                     theFileName,
                                                     sep = ""),
                                               firstDateToKeep,
                                               lastDateToKeep,
                                               traceThisRoutine = traceThisRoutine,
                                               prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testDiscardDataOutsideDateRange_US_vacc", theFileName, "\n")
  }
  
  return(foo1)
}

testDiscards <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testDiscards\n")
  }

  foo1 <- testDiscardTooNew("US_V_TEST.csv",
                            vaccColTypes(),
                            mdy("08-12-2021"),
                            traceThisRoutine = traceThisRoutine,
                            prepend = myPrepend)
  foo2 <- testDiscardTooOld("US_V_TEST.csv",
                            vaccColTypes(),
                            mdy("08-01-2021"),
                            traceThisRoutine = traceThisRoutine,
                            prepend = myPrepend)
  foo3 <- testDiscardDataOutsideDateRange("US_V_TEST.csv",
                                          vaccColTypes(),
                                          mdy("08-01-2021"),
                                          mdy("08-11-2021"),
                                          traceThisRoutine = traceThisRoutine,
                                          prepend = myPrepend)

  foo4 <- testDiscardTooNew("US_IR_TEST.csv",
                            myTSColTypes(),
                            mdy("5-31-2021"),
                            traceThisRoutine = traceThisRoutine,
                            prepend = myPrepend)
  foo5 <- testDiscardTooOld("US_IR_TEST.csv",
                            myTSColTypes(),
                            mdy("5-02-2021"),
                            traceThisRoutine = traceThisRoutine,
                            prepend = myPrepend)
  foo6 <- testDiscardDataOutsideDateRange("US_IR_TEST.csv",
                                          myTSColTypes(),
                                          mdy("05-02-2021"),
                                          mdy("05-30-2021"),
                                          traceThisRoutine = traceThisRoutine,
                                          prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testDiscards\n")
  }

  return(list(ORIG_123 = foo1$T0
              , RES1 = foo1$T1
              , RES2 = foo2$T1
              , RES3 = foo3$T1
              , ORIG_456 = foo4$T0
              , RES4 = foo4$T1
              , RES5 = foo5$T1
              , RES6 = foo6$T1
              ))
}


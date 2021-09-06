library(tidyverse)
library(lubridate)

# source("columnUtilities.R") (for vaccColTypes())
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
                                                 theTypes,
                                                 firstDateToKeep,
                                                 lastDateToKeep,
                                                 theFileName,
                                                 traceThisRoutine = FALSE, prepend = "") {
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
                                       theTypes,
                                       firstDateToDelete,
                                       theFileName,
                                       traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooNewDataFromAFile", theFileName, "\n")
  }
  
  return_me <- discardDataOutsideDateRangeFromAFile(thePath,
                                                    theTypes,
                                                    0,
                                                    firstDateToDelete - 1,
                                                    theFileName,
                                                    traceThisRoutine, myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardTooNewDataFromAFile", theFileName, "\n")
  }
  
  return(return_me)
}

discardTooOldDataFromAFile <- function(thePath,
                                       theTypes,
                                       firstDateToKeep,
                                       theFileName,
                                       traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooOldDataFromAFile", theFileName, "\n")
  }
  
  return_me <- discardDataOutsideDateRangeFromAFile(thePath,
                                                    theTypes,
                                                    firstDateToKeep,
                                                    Sys.Date() + 2,
                                                    theFileName,
                                                    traceThisRoutine, myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardTooOldDataFromAFile", theFileName, "\n")
  }
  
  return(return_me)
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
reinitTestDataFile <- function(theFileName,
                               traceThisRoutine = FALSE,
                               prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered reinitTestDataFile", theFileName, "\n")
  }
  
  system(paste("cp DATA/CACHE/", theFileName, " DATA/", sep = ""))
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving reinitTestDataFile", theFileName, "\n")
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
  
  foo1 <- discardTooNewDataFromAFile(paste("./DATA/", theFileName, sep=""),
                                     theTypes,
                                     firstDateToDelete,
                                     theFileName,
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
  
  foo1 <- discardTooOldDataFromAFile(paste("./DATA/", theFileName, sep=""),
                                     theTypes,
                                     firstDateToKeep,
                                     theFileName,
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
  
  foo1 <- discardDataOutsideDateRangeFromAFile(paste("./DATA/", theFileName, sep = ""),
                                               theTypes,
                                               firstDateToKeep,
                                               lastDateToKeep,
                                               theFileName,
                                               traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testDiscardDataOutsideDateRange_US_vacc", theFileName, "\n")
  }
  
  return(foo1)
}

# testSuite <- function(traceThisRoutine = FALSE, prepend = "") {
#   myPrepend = paste("  ", prepend, sep = "")  
#   if (traceThisRoutine) {
#     cat(file = stderr(), prepend, "Entered testSuite\n")
#   }
#   
#   foo1 <- testDiscardTooNew("US_V_TEST.csv",
#                             vaccColTypesç,
#                             mdy("08-12-2021"), 
#                             traceThisRoutine = traceThisRoutine,
#                             prepend = myPrepend)
#   foo2 <- testDiscardTooOld("US_V_TEST.csv",
#                             vaccColTypes√,
#                             mdy("08-01-2021"), 
#                             traceThisRoutine = traceThisRoutine,
#                             prepend = myPrepend)
#   foo3 <- testDiscardDataOutsideDateRange("US_V_TEST.csv",
#                                           vaccColTypes√,
#                                           mdy("08-01-2021"),
#                                           mdy("08-11-2021"),
#                                           traceThisRoutine = traceThisRoutine,
#                                           prepend = myPrepend)
# 
#   foo4 <- testDiscardTooNew("US_IR_TEST.csv",
#                             myTSColTypes,
#                             mdy("5-31-2021"),
#                             traceThisRoutine = traceThisRoutine,
#                             prepend = myPrepend)
#   foo5 <- testDiscardTooOld("US_IR_TEST.csv",
#                             myTSColTypes,
#                             mdy("5-02-2021"),
#                             traceThisRoutine = traceThisRoutine,
#                             prepend = myPrepend)
#   foo6 <- testDiscardDataOutsideDateRange("US_IR_TEST.csv",
#                                           myTSColTypes,
#                                           mdy("05-02-2021"),
#                                           mdy("05-30-2021"),
#                                           traceThisRoutine = traceThisRoutine,
#                                           prepend = myPrepend)
#   
#   if (traceThisRoutine) {
#     cat(file = stderr(), prepend, "Leaving testSuite\n")
#   }
# 
#   return(list(ORIG_123 = foo1$T0
#               , RES1 = foo1$T1
#               , RES2 = foo2$T1
#               , RES3 = foo3$T1
#               , ORIG_456 = foo4$T0
#               , RES4 = foo4$T1
#               , RES5 = foo5$T1
#               , RES6 = foo6$T1
#               ))
# }

zapFiles <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered zapFiles\n")
  }
  
  # for (aName in c("US_County_Confirmed.csv", "US_County_Deaths.csv",
  #                 "US_Deaths.csv",
  #                 "US_State_Case_Fatality_Ratio.csv", "US_State_Confirmed.csv",
  #                 "US_State_Deaths.csv", "US_State_Incident_Rate.csv",
  #                 "US_State_Testing_Rate.csv",
  #                 "US_State_Total_Test_Results.csv", "US_State_Vaccinations.csv",
  #                 "US_Testing_Rate.csv", "US_Total_Test_Results.csv",
  #                 "US_Vaccinations.csv")) {
  for (aName in c("US_State_Vaccinations.csv",
                  "US_Vaccinations.csv")) {
    
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Trying to remove Population column from", aName, "\n")
    }
    
    # traceNow <- traceThisRoutine & (aName %in% c("US_Case_Fatality_Ratio.csv",
    #                                              "US_Confirmed.csv",
    #                                              "US_Incident_Rate.csv"))
    maybePopData <- read_csv(paste("DATA/", aName, sep = ""), col_types = vaccColTypes())
    
    # if (traceNow) {
    #   conciseEndsOfTibbleRow(maybePopData, paste("maybePopData for", aName),
    #                          theKey = "Combined_Key",
    #                          keyValue = "US",
    #                          nFirst = 4, nLast = 2,
    #                          traceThisRoutine = traceNow, prepend = myPrepend)
    # }
    
    colNames <- names(maybePopData)
    if ("Population" %in% colNames) {
      noPopData <-  maybePopData %>%
        select(!Population)

      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Removed population column\n")
      }
      
      # if (traceNow) {
      #   conciseEndsOfTibbleRow(noPopData, paste("noPopData for", aName),
      #                          theKey = "Combined_Key",
      #                          keyValue = "US",
      #                          nFirst = 4, nLast = 2,
      #                          traceThisRoutine = traceNow, prepend = myPrepend)
      # }
    } else {
      noPopData <- maybePopData
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "No population column found\n")
      }
    }

    noLastDateData <- noPopData %>%
      discardDataOutsideDateRangeFromATibble(mdy("06/02/2021"),
                                             mdy("09/03/2021"),
                                             traceThisRoutine = FALSE, prepend = "")
    
    # if (traceNow) {
    #   conciseEndsOfTibbleRow(noLastDateData, paste("noLastDateData for", aName),
    #                          theKey = "Combined_Key",
    #                          keyValue = "US",
    #                          nFirst = 4, nLast = 2,
    #                          traceThisRoutine = traceThisRoutine, prepend = myPrepend)
    # }
    
    write_csv(noLastDateData, paste("DATA/", aName, sep = ""))

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

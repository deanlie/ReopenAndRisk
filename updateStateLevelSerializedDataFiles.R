
library(tidyverse)
library(lubridate)
library(stringi)
library(RCurl)

source("pathnameFunctions.R")
source("URLFunctions.R")
source("downloadJHUData.R")
source("columnUtilities.R")
source("rebuildDataFileForType.R")
source("verifyFileListLatestUpdates.R")

typesStateLevelData <- function(aDate) {
  c("Incident_Rate", "Case_Fatality_Ratio",
    "Total_Test_Results", "Testing_Rate")
}

sumIgnoreNA <- function(x) {
  sum(x, na.rm = TRUE)
}

discardNewerDataFromListOfTibbles <- function(listOfTibbles,
                                              firstDateToDelete,
                                              traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardNewerDataFromListOfTibbles\n")
  }
  
  for (memberName in names(listOfTibbles)) {
    columnNames <- names(listOfTibbles[[memberName]])
    dateNames <- columnNames[grep("^1?[0-9]/[1-3]?[0-9]/2[0-9]$", columnNames)]
    keepTheseNames <- columnNames[grep("^1?[0-9]/[1-3]?[0-9]/2[0-9]$", columnNames, invert = TRUE)]
    for (aName in dateNames) {
      if (mdy(aName) < firstDateToDelete) {
        keepTheseNames <- c(keepTheseNames, aName)
      }
    }
    listOfTibbles[[memberName]] <- listOfTibbles[[memberName]] %>%
      select(any_of({keepTheseNames}))
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardNewerDataFromListOfTibbles\n")
  }
  return(listOfTibbles)
}

discardTooNewDataFromStateTibbles <- function(existingStateTibbles,
                                              firstDateToDelete,
                                              traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooNewDataFromStateTibbles\n")
  }
  
  postSelectionTibbles <- discardNewerDataFromListOfTibbles(existingStateTibbles,
                                                            firstDateToDelete,
                                                            traceThisRoutine = traceThisRoutine,
                                                            prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardTooNewDataFromStateTibbles\n")
  }
  return(existingStateTibbles)
}

updateExistingStateDataFilesForTypes <- function(existingST,
                                                 nDates = nDates,
                                                 stopNDaysBeforePresent = stopNDaysBeforePresent,
                                                 traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend) {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateExistingStateDataFilesForTypes\n")
  }
  
  # Drop the too-early dates
  existingST <- discardOutdatedDataFromStateTibbles(existingST,
                                                    keepUpToNDaysBeforePresent = nDates,
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
  
  # Find dates which are in sequence
  commonNames <- commonNamesInStateTibbles(existingST)
  lastDate <- Sys.Date() - stopNDaysBeforePresent
  firstDate <- lastDate - nDates
  
  newST <- existingST
  
  # firstDate should be mdy(commonNames[3]), etc. Check everything!
  lastNameIndex <- length(commonNames)
  for (i in 0:(nDates - 1)) {
    if (((i + 3) > lastNameIndex) | (mdy(commonNames[3 + i]) != (firstDate + i))){
      # Add more dates up to latest
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Must add data for",
            format.Date(firstDate + i, "%m/%d/%y"), "through",
            format.Date(lastDate, "%m/%d/%y"), "\n")
      }
      newST <- discardTooNewDataFromStateTibbles(existingStateTibbles,
                                                 firstDate + i,
                                                 traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend)
      
      newST <- addDateRangeToStateDataFilesForTypes(newST, firstDate + i, lastDate,
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
      break
    }
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateExistingStateDataFilesForTypes\n")
  }
  
  return(newST)
}

updateStateDataFilesForTypes <- function(nDates = 60, stopNDaysBeforePresent = 0,
                                         traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateStateDataFilesForTypes\n")
  }
  existingST <- openExistingStateTibbles()
  commonNames <- commonNamesInStateTibbles(existingST)
  lastDate <- Sys.Date() - stopNDaysBeforePresent
  firstDate <- lastDate - nDates
  desiredFirstName <- formatDateForColumnName(firstDate)
  if (desiredFirstName %in% commonNames) {
    if (traceThisRoutine) {
      cat(file = stderr(), "We have", desiredFirstName, "in commonNames.\n")
    }
    newStateTibbles <- updateExistingStateDataFilesForTypes(existingST,
                                                            nDates = nDates,
                                                            stopNDaysBeforePresent = stopNDaysBeforePresent,
                                                            traceThisRoutine = traceThisRoutine,
                                                            prepend = myPrepend)
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), "We don't have", desiredFirstName, "in commonNames, will rebuild from scratch\n")
    }
    newStateTibbles <- rebuildStateDataFilesForTypes(nDates = nDates,
                                                     stopNDaysBeforePresent = stopNDaysBeforePresent,
                                                     traceThisRoutine = traceThisRoutine,
                                                     prepend = myPrepend)
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateStateDataFilesForTypes\n")
  }
  return(newStateTibbles)
}

updatePopulationEstimateData <- function(dailyStateData,
                                         traceThisRoutine = FALSE, prepend = "") {
  # The data set I worked from allowed estimating population two ways.
  # Population Estimate 1 = 100000 * Confirmed / Incident_Rate
  # Population Estimate 2 = 100000 * Total_Test_Results / Testing_Rate
  # While developing this I compared those two estimates.
  # The difference in the two, when it could be computed, was always
  # 0, -1, or +1 -- that is, it seemed to be a roundoff error.
  # Because there were 0 confirmed cases in American Samoa, estimate 1
  # could not be computed for it. I therefore decided to eliminate
  # estimate 1 and estimate the population of all items as
  # 100000 * Total_Test_Results / Testing_Rate.

  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updatePopulationEstimateData\n")
  }
  
  # daily data file passed in dailyStateData
  newPopulationTibble <- dailyStateData %>%
    filter(!str_detect(Province_State, "Princess")) %>%
    mutate(Combined_Key = paste(Province_State, ", US", sep=""),
           Population = as.integer(100000 * Total_Test_Results / Testing_Rate),
           .keep = "none")
  
  newUSPopTibble <- newPopulationTibble %>%
    summarise(Combined_Key = "US", across(.cols = "Population", sumIgnoreNA))
  
  newData <- bind_rows(newUSPopTibble, newPopulationTibble)
  
  # Save new population estimate file
  
  write_csv(newData, "./DATA/US_State_Population_Est.csv")
  
  if (traceThisRoutine) {
    cxn <- file("./DEVELOPMENT/fileWriteLog.txt", "a")
    cat(file = cxn,
        "Wrote ./DATA/US_State_Population_Est.csv",
        "in updatePopulationEstimateData\n")
    close(cxn)
    cat(file = stderr(), prepend, "Leaving updatePopulationEstimateData\n")
  }
  return(newData)
}

makeInitialStateLevelData <- function(nDates = 60,
                                      traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered makeInitialStateLevelData\n")
  }
  
  # Download <first_date>_us.csv from Github if necessary
  firstDate <- Sys.Date() - nDates
  
  columnDate <- paste(month(firstDate),
                      day(firstDate),
                      (year(firstDate) - 2000), sep="/")

  updateTibble <- downloadStateLevelUpdateata(firstDate)
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, paste("downloaded update tibble", sep = ""))
  }

  popEstimate <- updatePopulationEstimateData(updateTibble,
                                              traceThisRoutine = traceThisRoutine,
                                              prepend = myPrepend)
  if (traceThisRoutine) {
    cxn <- file("./DEVELOPMENT/fileWriteLog.txt", "a")
  }
  
  aList <- typesStateLevelData()
  for (aType in aList) {
    newTibble <- updateTibble %>%
      mutate(Combined_Key = paste(Province_State, ", US", sep=""),
             Population = as.integer(100000 * Confirmed / Incident_Rate),
             "{columnDate}" := .data[[aType]],
             .keep = "none")

    # TODO:
    # Test Puerto Rico and DC carefully in all paths of UI interaction
    
    write_csv(newTibble, paste("./DATA/US_State_", aType, ".csv", sep=""))
    if (traceThisRoutine) {
      cat(file = cxn, "Wrote",
          paste("./DATA/US_State_", aType, ".csv", sep=""),
          "in makeInitialStateLevelData\n")
      cat(file = stderr(), myPrepend, "Wrote",
          paste("./DATA/US_State_", aType, ".csv", sep=""), "\n")
      cat(file = stderr(), myPrepend, "It had", dim(newTibble)[2], "columns\n")
    }
    
    newUSTibble <- newTibble %>%
      summarise(Province_State = "", Combined_Key = "US",
                across(matches("^Pop|^[1-9]+/"), sumIgnoreNA))

    write_csv(newUSTibble, paste("./DATA/US_", aType, ".csv", sep=""))
    if (traceThisRoutine) {
      cat(file = cxn, "Wrote",
          paste("./DATA/US_", aType, ".csv", sep=""),
          "in makeInitialStateLevelData\n")
      cat(file = stderr(), myPrepend, "Wrote",
          paste("./DATA/US_", aType, ".csv", sep=""), "\n")
      cat(file = stderr(), myPrepend, "It had", dim(newUSTibble)[2], "columns\n")
    }
  }
  if (traceThisRoutine) {
    close(cxn)
    cat(file = stderr(), prepend, "Leaving makeInitialStateLevelData\n")
  }
}

updateSerializedDataFilesAsNecessary <- function(staticDataQ = FALSE,
                                                 traceThisRoutine = FALSE,
                                                 prepend = "CALLER??") {
  myPrepend <- paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateSerializedDataFilesAsNecessary\n")
  }

  filesUpdatedInSerializedCall <- c("US_Testing_Rate.csv",
                                    "US_State_Testing_Rate.csv",
                                    "US_Incident_Rate.csv",
                                    "US_State_Incident_Rate.csv",
                                    "US_Case_Fatality_Ratio.csv",
                                    "US_State_Case_Fatality_Ratio.csv",
                                    "US_Total_Test_Results.csv",
                                    "US_State_Total_Test_Results.csv")
  if(staticDataQ) {
    desiredLatestDate <- mdy("11/24/21")
    dataDir <- "./DATA/STATIC/"
  } else {
    desiredLatestDate <- expectedLatestUpdateDataDate()
    dataDir <- "./DATA/"
  }
  
  mismatches <- verifyFileListLatestUpdates(filesUpdatedInSerializedCall,
                                            desiredLatestDate,
                                            dataDir,
                                            traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)

  if (traceThisRoutine) {
    cxn <- file("./DEVELOPMENT/fileWriteLog.txt", "a")
  }

  while (dim(mismatches)[1] > 0) {
    overallLastUpdate <- mismatches$lastUpdate[1]
    firstDate <- overallLastUpdate + 1
    firstGroup <- filter(mismatches, lastUpdate <= {{overallLastUpdate}})
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, dim(firstGroup)[1],
          "files only updated to", format.Date(overallLastUpdate), "\n")
    }
    remainingMismatches <- filter(mismatches, lastUpdate >= {{firstDate}})
    if (dim(remainingMismatches)[1] > 0) {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, dim(remainingMismatches)[1],
            "files remaining\n")
      }
      nextDate <- remainingMismatches$lastUpdate[1]
    } else {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "... that's all.\n")
      }
      nextDate <- desiredLatestDate
    }
    nDays <- nextDate - firstDate
    for (addDay in 0:nDays) {
      updateDate <- firstDate + addDay
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend,
            "Updating to", format.Date(updateDate), "\n")
      }
      # 
      # # Check the last state level serialized data file for last date;
      # if (!dataIsCurrent("./DATA/US_Testing_Rate.csv")) {
      #   options(show.error.messages = traceThisRoutine)
      #   US_Testing_Rate <- try(read_csv("./DATA/US_Testing_Rate.csv",
      #                                   col_types = justCKColTypes()))
      #   options(show.error.messages = TRUE)
      #   # Update is required
      #   if (class(US_Testing_Rate)[1] == "try-error") {
      #     # There's no old data of the type we want.
      #     if (traceThisRoutine) {
      #       cat(file = stderr(), myPrepend, "No US_Testing_Rate.csv found on entry\n")
      #     }
      #     makeInitialStateLevelData(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
      #     # Now we should be able to get some data
      #     options(show.error.messages = TRUE)
      #     US_Testing_Rate <- try(read_csv("./DATA/US_Testing_Rate.csv",
      #                                     col_types = justCKColTypes()))
      #   } 
      #   # We have US_Testing_Rate for that type -- but it may not be up-to-date
      #   if (traceThisRoutine) {
      #     cat(file = stderr(), myPrepend, "We have a US_Testing_Rate file, but it may not be current.\n")
      #   }
      #   # Get date range needed
      #   colNames <- names(US_Testing_Rate)
      #   lastName <- colNames[length(colNames)]
      #   if (traceThisRoutine) {
      #     cat(file = stderr(), myPrepend, "Latest column is", lastName, "\n")
      #   }
      #   
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend,
            "Update date is ", format.Date(updateDate), "\n")
      }
      
      columnDate <- paste(month(updateDate),
                          day(updateDate),
                          (year(updateDate) - 2000), sep="/")
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Getting data for update to", columnDate, "\n")
      }
      # Read or download update file for that date
      
      updateTibble <- downloadStateLevelUpdateData(updateDate,
                                                   traceThisRoutine = traceThisRoutine,
                                                   prepend = myPrepend)

      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, paste("downloaded the update data for",
                                              columnDate, "\n", sep = ""))
      }

      if ("tbl" %in% class(updateTibble)) {
        popEstimate <- updatePopulationEstimateData(updateTibble,
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
        
        # We need two lists as in createUSNewTypeDataTimeSeries.R
        aList <- typesStateLevelData()
        for (i in 1:length(aList)) {
          thatType <- aList[i]
          prevType <- aList[i]
          oldLocalDataPath <- paste("./DATA/US_State_", prevType, ".csv", sep = "")
          newLocalStateDataPath <- paste("./DATA/US_State_", thatType, ".csv", sep = "")
          if (traceThisRoutine) {
            cat(file = stderr(), myPrepend, "Updating ", oldLocalDataPath,
                "to", newLocalStateDataPath,"\n")
            cat(file = stderr(), myPrepend,
                paste("before try(read_csv(", oldLocalDataPath, ")\n", sep = ""))
          }
          options(show.error.messages = traceThisRoutine)
          
          oldData <- try(read_csv(oldLocalDataPath,
                                  col_types = justCKColTypes()))
          if (traceThisRoutine) {
            cat(file = stderr(), myPrepend,
                paste("after try(read_csv(", oldLocalDataPath, "))\n", sep=""))
          }
          options(show.error.messages = TRUE)
          # Update is required
          if (traceThisRoutine) {
            if (class(oldData)[1] == "try-error") {
              cat(file = stderr(), myPrepend,
                  paste("read_csv(", oldLocalDataPath,") failed\n", sep=""))
            }
            cat(file = stderr(), myPrepend, "last col of", oldLocalDataPath,
                "is", names(oldData)[length(names(oldData))], "\n")
            cat(file = stderr(), myPrepend,
                "newLocalStateDataPath = ", newLocalStateDataPath, "\n")
          }
          joinTibble <- updateTibble %>%
            mutate(Combined_Key = paste(Province_State, ", US", sep=""),
                   "{columnDate}" := .data[[thatType]],
                   .keep = "none")
          newData <- left_join(oldData, joinTibble, by="Combined_Key")
          write_csv(newData, newLocalStateDataPath)
          if (traceThisRoutine) {
            cat(file = stderr(), myPrepend,
                "Wrote", newLocalStateDataPath, "with last col",
                names(newData)[length(names(newData))],
                "in updateSerializedDataFilesAsNecessary\n")
            cat(file = cxn,
                "Wrote", newLocalStateDataPath,
                "in updateSerializedDataFilesAsNecessary\n")
          }
        }
      } else {
        break;
      }
    }
    mismatches <- remainingMismatches
  }

  US_State_Total_Test_Results <- read_csv("./DATA/US_State_Total_Test_Results.csv",
                                          show_col_types = FALSE)
  US_Total_Test_Results <- rebuildUSDataFileForTypeAsSummary(US_State_Total_Test_Results,
                                                             "Total_Test_Results",
                                                             traceThisRoutine = traceThisRoutine,
                                                             prepend = myPrepend)

  US_Deaths <- read_csv("./DATA/US_Deaths.csv",
                        show_col_types = FALSE)
  US_Confirmed <- read_csv("./DATA/US_Confirmed.csv",
                           show_col_types = FALSE)
  
  US_Case_Fatality_Ratio <- rebuildUSDataFileForTypeFromProperData(US_Deaths,
                                                                   US_Confirmed,
                                                                   "Case_Fatality_Ratio",
                                                                   traceThisRoutine = traceThisRoutine,
                                                                   prepend = myPrepend)

  US_Incident_Rate <- rebuildUSDataFileForTypeByNormalizing(US_Confirmed,
                                                            "Incident_Rate",
                                                            perHowMany = 100000,
                                                            traceThisRoutine = traceThisRoutine,
                                                            prepend = myPrepend)

  US_Testing_Rate <- rebuildUSDataFileForTypeByNormalizing(US_Total_Test_Results,
                                                           "Testing_Rate",
                                                           perHowMany = 100,
                                                           traceThisRoutine = traceThisRoutine,
                                                           prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateSerializedDataFilesAsNecessary\n")
  }
}

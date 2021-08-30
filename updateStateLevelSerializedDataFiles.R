
library(tidyverse)
library(lubridate)
library(stringi)
library(RCurl)

source("./dateFormatRoutines.R")
source("./dataIsCurrent.R")
source("./downloadJHUData.R")

typesStateLevelData <- function(aDate) {
  newTypes <- c("Incident_Rate", "Case_Fatality_Ratio",
                "Total_Test_Results", "Testing_Rate")

  list(thatDateTypes = newTypes, prevDateTypes = newTypes)
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
  
  # for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
  #   theNames <- names(existingStateTibbles[[aType]])
  #   newNames <- c("Combined_Key", "Population")
  #   for (aName in theNames) {
  #     if (aName != "Combined_Key" & aName != "Population") {
  #       if (mdy(aName) < firstDateToDelete) {
  #         newNames <- c(newNames, aName)
  #       }
  #     }
  #   }
  #   existingStateTibbles[[aType]] <- existingStateTibbles[[aType]] %>%
  #     select(any_of({newNames}))
  # }
  
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
    cat(file = stderr(), prepend, "Entered updateExistingStateDataFilesForTypes\n")
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

updatePopulationEstimateData <- function(aDate, dailyStateData,
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
    cat(file = stderr(), prepend, "Leaving updatePopulationEstimateData\n")
  }
  return(newData)
}

makeInitialStateLevelData <- function(nDates = 90,
                                      traceThisRoutine = FALSE, prepend = "") {
  # stop("Per request!")
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered makeInitialStateLevelData\n")
  }
  
  # Download <first_date>_us.csv from Github if necessary
  firstDate <- Sys.Date() - nDates
  
  columnDate <- paste(month(firstDate),
                      day(firstDate),
                      (year(firstDate) - 2000), sep="/")

  # If we don't have the data as a local file, download it  
  if (!file.exists(pathnameOfStateLevelUpdateDataForDate(firstDate))) {
    # No local file! Let's see if we can download it...
    downloadAndSaveStateLevelUpdateData(firstDate)
    # # START
    # # OUCH Put downloads in a download module with all the error handling
    # oldShowError <- getOption(show.error.messages)
    # options(show.error.messages) = TRUE
    # # No local file! Let's see if we can download it...
    # if (url.exists(updateStateLevelDataForDate_URL(firstDate))) {
    #   if (traceThisRoutine) {
    #     cat(file = stderr(), myPrepend, "Remote file",
    #         updateStateLevelDataForDate_URL(firstDate),
    #         "exists, will try to download\n")
    #   }
    #   # Get the first file
    #   updateTibble <- try(read_csv(updateStateLevelDataForDate_URL(firstDate),
    #                                col_types = cols(.default = col_double(),
    #                                                 Province_State = col_character(),
    #                                                 Country_Region = col_character(),
    #                                                 Last_Update = col_datetime(format = ""),
    #                                                 People_Hospitalized = col_logical(),
    #                                                 ISO3 = col_character(),
    #                                                 Hospitalization_Rate = col_logical())))
    #   if (class(updateTibble)[1] == "try-error") {
    #     cat(file = stderr(), myPrepend,
    #         "download of", updateStateLevelDataForDate_URL(firstDate), "failed\n")
    #     cat(file = stderr(), myPrepend, "FATAL ERROR\n")
    #     stop()
    #   }
    #   write_csv(updateTibble, pathnameOfStateLevelUpdateDataForDate(firstDate))
    #   cat(file = stderr(), myPrepend,
    #       "Downloaded and wrote", pathnameOfStateLevelUpdateDataForDate(firstDate), "\n")
    # } else {
    #   cat(file = stderr(), myPrepend,
    #       "url.exists returns FALSE:", updateStateLevelDataForDate_URL(firstDate), "\n")
    #   cat(file = stderr(), myPrepend, "FATAL ERROR\n")
    #   stop()
    # }
    # #STOP
  }
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend,
        paste("before try(read_csv(", pathnameOfStateLevelUpdateDataForDate(firstDate), "))\n", sep = ""))
  }
  updateTibble <- try(read_csv(pathnameOfStateLevelUpdateDataForDate(firstDate)))
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, paste("after try(read_csv(",
                                          pathnameOfStateLevelUpdateDataForDate(firstDate),
                                          "))\n", sep = ""))
  }
  if (class(updateTibble)[1] == "try-error") {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend,
          "Can't read ", pathnameOfStateLevelUpdateDataForDate(firstDate), "\n")
    }
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "We were able to read",
          pathnameOfStateLevelUpdateDataForDate(firstDate),
          "from local disk/SSD\n")
    }
  }
  
  popEstimate <- updatePopulationEstimateData(firstDate, updateTibble,
                                              traceThisRoutine = traceThisRoutine,
                                              prepend = myPrepend)

  aList <- typesStateLevelData(firstDate)
  for (i in 1:length(aList$thatDateTypes)) {
    aType <- aList$thatDateTypes[i]
    newTibble <- updateTibble %>%
      mutate(Combined_Key = paste(Province_State, ", US", sep=""),
             Population = as.integer(100000 * Confirmed / Incident_Rate),
             "{columnDate}" := .data[[aType]],
             .keep = "none")
    
    # TODO:
    # Test Puerto Rico and DC carefully in all paths of UI interaction
    
    write_csv(newTibble, paste("./DATA/US_State_", aType, ".csv", sep=""))
    
    newUSTibble <- newTibble %>%
      summarise(Province_State = "", Combined_Key = "US",
                across(matches("^Pop|^[1-9]+/"), sumIgnoreNA))
    
    write_csv(newUSTibble, paste("./DATA/US_", aType, ".csv", sep=""))
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving makeInitialStateLevelData\n")
  }
}

updateStateLevelSerializedDataFilesAsNecessary <- function(traceThisRoutine = FALSE, prepend = "CALLER??") {
  myPrepend <- paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered updateStateLevelSerializedDataFilesAsNecessary\n")
  }

  # Check the last state level serialized data file for last date;
  if (!dataIsCurrent("./DATA/US_Testing_Rate.csv")) {
    options(show.error.messages = traceThisRoutine)
    US_Testing_Rate <- try(read_csv("./DATA/US_Testing_Rate.csv",
                                    col_types = cols(.default = col_double(),
                                                     Combined_Key = col_character())))
    options(show.error.messages = TRUE)
    # Update is required
    if (class(US_Testing_Rate)[1] == "try-error") {
      # There's no old data of the type we want.
      makeInitialStateLevelData(traceThisRoutine = traceThisRoutine, prepend = myPrepend)
      # Now we should be able to get some data
      options(show.error.messages = TRUE)
      US_Testing_Rate <- try(read_csv("./DATA/US_Testing_Rate.csv",
                                      col_types = cols(.default = col_double(),
                                                       Combined_Key = col_character())))
    } 
    # We have US_Testing_Rate for that type -- but it may not be up-to-date
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "We have a data file, but it may not be current.\n")
    }
    # Get date range needed
    colNames <- names(US_Testing_Rate)
    lastName <- colNames[length(colNames)]
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Latest data is from", lastName, "\n")
    }

    lastDate <- mdy(lastName)
    updateDate <- lastDate + 1

    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Update date is ", lastName, "\n")
    }

    while (updateDate < (today("EST"))) {
      columnDate <- paste(month(updateDate),
                          day(updateDate),
                          (year(updateDate) - 2000), sep="/")
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Getting data for update to", columnDate, "\n")
      }
      # Read or download update file for that date
      
      if (!file.exists(pathnameOfStateLevelUpdateDataForDate(updateDate))) {
        # No local file! Let's see if we can download it...
        downloadAndSaveStateLevelUpdateData(updateDate,
                                            traceThisRoutine = traceThisRoutine,
                                            prepend = myPrepend)
      }
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend,
            paste("before try(read_csv(", pathnameOfStateLevelUpdateDataForDate(updateDate), "))\n", sep = ""))
      }
      updateTibble <- try(read_csv(pathnameOfStateLevelUpdateDataForDate(updateDate),
                                   col_types = cols(.default = col_double(),
                                                    Province_State = col_character(),
                                                    Country_Region = col_character(),
                                                    Last_Update = col_datetime(format = ""),
                                                    People_Hospitalized = col_logical(),
                                                    ISO3 = col_character(),
                                                    Hospitalization_Rate = col_logical())))
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, paste("after try(read_csv(",
                                              pathnameOfStateLevelUpdateDataForDate(updateDate),
                                              "))\n", sep = ""))
      }
      if (class(updateTibble)[1] == "try-error") {
        if (traceThisRoutine) {
          cat(file = stderr(), myPrepend,
              "Can't read ", pathnameOfStateLevelUpdateDataForDate(updateDate), "\n")
        }
        cat(file = stderr(), myPrepend, "FATAL ERROR -- UPDATE DATA NOT FOUND\n")
        stop()
      } else {
        if (traceThisRoutine) {
          cat(file = stderr(), myPrepend, "We were able to read",
              pathnameOfStateLevelUpdateDataForDate(updateDate),
              "from local disk/SSD\n")
        }
      }
        # #START
        # if (url.exists(updateStateLevelDataForDate_URL(updateDate))) {
        #   if (traceThisRoutine) {
        #     cat(file = stderr(), myPrepend, "Remote file ",
        #         updateStateLevelDataForDate_URL(updateDate),
        #         "exists, will try to download\n")
        #   }
        #   # Get the first file
        #   updateTibble <- read_csv(updateStateLevelDataForDate_URL(updateDate),
        #                            col_types = cols(.default = col_double(),
        #                                             Province_State = col_character(),
        #                                             Country_Region = col_character(),
        #                                             Last_Update = col_datetime(format = ""),
        #                                             People_Hospitalized = col_logical(),
        #                                             ISO3 = col_character(),
        #                                             Hospitalization_Rate = col_logical()))
        #   if (traceThisRoutine) {
        #     cat(file = stderr(), myPrepend, "OK, we downloaded",
        #       updateStateLevelDataForDate_URL(updateDate), "\n")
        #   }
        #   write_csv(updateTibble, pathnameOfStateLevelUpdateDataForDate(updateDate))
        #   if (traceThisRoutine) {
        #     cat(file = stderr(), myPrepend, "... and wrote",
        #         pathnameOfStateLevelUpdateDataForDate(updateDate), "\n")
        #   }
        # } else {
        #   if (traceThisRoutine) {
        #     cat(file = stderr(), myPrepend, "Remote file ",
        #         updateStateLevelDataForDate_URL(updateDate),
        #         "not accessible or does not exist","\n")
        #   }
        # }
        # #STOP
      if ("tbl" %in% class(updateTibble)) {
        popEstimate <- updatePopulationEstimateData(updateDate, updateTibble,
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
        
        # We need two lists as in createUSNewTypeDataTimeSeries.R
        aList <- typesStateLevelData(updateDate)
        for (i in 1:length(aList$thatDateTypes)) {
          thatType <- aList$thatDateTypes[i]
          prevType <- aList$prevDateTypes[i]
          oldLocalDataPath <- paste("./DATA/US_State_", prevType, ".csv", sep = "")
          newLocalDataPath <- paste("./DATA/US_State_", thatType, ".csv", sep = "")
          newLocalUSDataPath <- paste("./DATA/US_", thatType, ".csv", sep = "")
          if (traceThisRoutine) {
            cat(file = stderr(), myPrepend, "Updating ", oldLocalDataPath, "to", newLocalDataPath,"\n")
            cat(file = stderr(), myPrepend,
                paste("before try(read_csv(", oldLocalDataPath, ")\n", sep = ""))
          }
          options(show.error.messages = traceThisRoutine)
          oldData <- try(read_csv(oldLocalDataPath,
                                  col_types = cols(.default = col_double(),
                                                   Combined_Key = col_character())))
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
              cat(file = stderr(), myPrepend, "newLocalDataPath = ", newLocalDataPath, "\n")
            }
          }
          joinTibble <- updateTibble %>%
            mutate(Combined_Key = paste(Province_State, ", US", sep=""),
                   "{columnDate}" := .data[[thatType]],
                   .keep = "none")
          newData <- left_join(oldData, joinTibble, by="Combined_Key")
          write_csv(newData, newLocalDataPath)
        
          newUSData <- newData %>%
            summarise(Province_State = "", Combined_Key = "US",
                      across(matches("^Pop|^[1-9]+/"), sumIgnoreNA))
          write_csv(newUSData, newLocalUSDataPath)
        }
        updateDate <- updateDate + 1
      } else {
        break;
      }
    }
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving updateStateLevelSerializedDataFilesAsNecessary\n")
  }
}

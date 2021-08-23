discardTooNewDataFromATibble <- function(aTibble,
                                         firstDateToDelete,
                                         traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooNewDataFromATibble\n")
  }

  theNames <- names(aTibble)
  newNames <- c("Combined_Key")
  for (aName in theNames) {
    if (aName != "Combined_Key") {
      if (mdy(aName) < firstDateToDelete) {
        newNames <- c(newNames, aName)
      }
    }
  }
  resultTibble <- aTibble %>%
    select(any_of({newNames}))
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardTooNewDataFromATibble\n")
  }
}

discardTooNewDataFromStateTibbles <- function(existingStateTibbles,
                                              firstDateToDelete,
                                              traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardTooNewDataFromStateTibbles\n")
  }

  for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
    if(traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Processing type", aType, "\n")
    }
    
    aTibble <- existingStateTibbles[[aType]]
    
    resultTibble <- discardTooNewDataFromATibble(aTibble,
                                                 firstDateToDelete,
                                                 traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend)
    
    existingStateTibbles[[aType]] <- resultTibble
  }
  
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
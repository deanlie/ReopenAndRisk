# Rebuild US and State data files for data in daily files:
# Incident_Rate, Total_Test_Results, Case_Fatality_Ratio,
# Testing_Rate

# Daily update data is still available in ReopenAndRisk/DATA/mm-dd-2021.csv
# from 04-01-2021 through 07-21-2021

library(tidyverse)
library(RCurl)
library(glue)

source("dateFormatRoutines.R")
source("columnUtilities.R")

localDataDirectory <- function() {
  "./DATA/"
}

newStateDataPathForType <- function(aType) {
  paste(localDataDirectory(), "US_State_", aType, ".csv", sep = "")
}

newUSDataPathForType <- function(aType) {
  paste(localDataDirectory(), "US_", aType, ".csv", sep = "")
}

dataPathForDate <- function(aDate) {
  paste(localDataDirectory(), jhuFileDateString(aDate), ".csv",
        sep = "")
}

sumIgnoreNA <- function(x) {
  sum(x, na.rm = TRUE)
}

dumpTibbleStart <- function(aTibble, itsName, prepend = "") {
  cat(file = stderr(), prepend, itsName, "\n")
  cat(file = stderr(), prepend, paste(names(aTibble)[1:8]), "\n")
  cat(file = stderr(), prepend, as.character(aTibble[1,1]), as.integer(aTibble[1,2]),
      round(as.double(aTibble[3]), digits = 1), round(as.double(aTibble[4]), digits = 1),
      round(as.double(aTibble[5]), digits = 1), round(as.double(aTibble[6]), digits = 1),
      round(as.double(aTibble[7]), digits = 1), round(as.double(aTibble[8]), digits = 1),
      "\n\n")
}

collectDateConstantColumns <- function(stateName, fileDate) {
  dataFilePath <- dataPathForDate(fileDate)
  newStateDataTibble <- read_csv(dataFilePath,
                                 col_types = dataFileColTypes()) %>% 
    select(Province_State, Country_Region, Lat, Long_, FIPS, UID, ISO3) %>%
    filter(Province_State == !!stateName)
}

collectConstantColumnsForState <- function(stateName) {
  nDates = 60
  firstDate <- Sys.Date() - nDates
  
  buildingConstDataTibble <- tibble(Province_State = NA, Country_Region = NA,
                                    Lat = NA, Long_ = NA, FIPS = NA,
                                    UID = NA, ISO3 = NA)
  
  for (i in 0:(nDates - 1)) { # 0:59
    columnDate <- firstDate + i
    
    if (columnDate >= Sys.Date()) {
      print(paste("Up to", as.character(Sys.Date()), "with i", i))
      break;
    }
    
    newData <- collectDateConstantColumns(stateName, columnDate)
    buildingConstDataTibble <- bind_rows(buildingConstDataTibble, newData)
  }
  buildingConstDataTibble
}

openExistingStateTibbles <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered openExistingStateTibbles\n")
  }

  existingStateTibbles <- list(Total_Test_Results  = NA,
                               Case_Fatality_Ratio = NA,
                               Incident_Rate       = NA,
                               Testing_Rate        = NA)
  for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "aType =", aType, "path =", newStateDataPathForType(aType), "\n")
    }
    existingStateTibbles[[aType]] <- read_csv(newStateDataPathForType(aType),
                                              col_types = justCKColTypes())
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "read", newStateDataPathForType(aType), "\n")
    }
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving openExistingStateTibbles\n")
  }

  return(existingStateTibbles)
}

discardOlderDataFromListOfTibbles <- function(listOfTibbles,
                                              keepFromNDaysBeforePresent = 60,
                                              traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardOlderDataFromListOfTibbles\n")
  }
  
  firstDateToKeep <- Sys.Date() - keepFromNDaysBeforePresent
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "firstDateToKeep = ", format.Date(firstDateToKeep, "%m/%d/%y"), "\n")
  }
  
  for (memberName in names(listOfTibbles)) {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "tibble", memberName, "\n")
    }
    columnNames <- names(listOfTibbles[[memberName]])
    dateNames <- columnNames[grep("^1?[0-9]/[1-3]?[0-9]/2[0-9]+$", columnNames)]
    keepTheseNames <- columnNames[grep("^1?[0-9]/[1-3]?[0-9]/2[0-9]+$", columnNames, invert = TRUE)]
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "before accum, keepTheseNames =", keepTheseNames, "\n")
      extraPrepend <- paste("  ", myPrepend, sep = "")
    }
    for (aName in dateNames) {
      if (mdy(aName) >= firstDateToKeep) {
        if (traceThisRoutine) {
          # cat(file = stderr(), myPrepend)
          cat(file = stderr(), extraPrepend, "accumulating aName = ", aName, "\n")
        }
        keepTheseNames <- c(keepTheseNames, aName)
      }
    }
    if (traceThisRoutine) {
      # cat(file = stderr(), myPrepend)
      cat(file = stderr(), myPrepend, "tibble", memberName,
          "after accum, keepTheseNames =", keepTheseNames, "\n")
    }
    listOfTibbles[[memberName]] <- listOfTibbles[[memberName]] %>%
      select(any_of({keepTheseNames}))
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardOlderDataFromListOfTibbles\n")
  }
  return(listOfTibbles)
}

discardOutdatedDataFromStateTibbles <- function(existingStateTibbles,
                                                keepUpToNDaysBeforePresent = 60,
                                                traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardOutdatedDataFromStateTibbles\n")
  }
  for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "aType =", aType, "\n")
    }
    theNames <- names(existingStateTibbles[[aType]])
    newNames <- c("Combined_Key", "Province_State")
    for (aName in theNames) {
      if (aName != "Combined_Key" & aName != "Population" & aName != "Province_State") {
        if (traceThisRoutine) {
          cat(file = stderr(), myPrepend, "aName =", aName, "\n")
        }
        if (mdy(aName) >= (Sys.Date() - keepUpToNDaysBeforePresent)) {
          newNames <- c(newNames, aName)
        }
      }
    }
    existingStateTibbles[[aType]] <- existingStateTibbles[[aType]] %>%
      select(any_of({newNames}))
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardOutdatedDataFromStateTibbles\n")
  }
  
  return(existingStateTibbles)
}

commonNamesInStateTibbles <- function(existingStateTibbles) {
  theNames <- list(Total_Test_Results  = NA,
                   Case_Fatality_Ratio = NA,
                   Incident_Rate       = NA,
                   Testing_Rate        = NA)
  for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
    theNames[[aType]] <- names(existingStateTibbles[[aType]])
  }
  commonNames <- intersect(intersect(theNames$Total_Test_Results,
                                     theNames$Case_Fatality_Ratio),
                           intersect(theNames$Incident_Rate,
                                     theNames$Testing_Rate))
  
  return(commonNames)
}

uniformizeDatesOfStateTibbles <- function(existingStateTibbles) {
  commonNames <- commonNamesInStateTibbles(existingStateTibbles)
  for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
    existingStateTibbles[[aType]] <- existingStateTibbles[[aType]] %>%
      select(any_of({commonNames}))
  }
  return(existingStateTibbles)
}

addDateToStateDataFilesForTypes <- function(newST, columnDate,
                                            traceThisRoutine = FALSE,
                                            prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered addDateToStateDataFilesForTypes\n")
  }

  dataFilePath <- dataPathForDate(columnDate)
  columnName <- formatJHUDateForColumnName(jhuFileDateString(columnDate))
  
  newStateDataTibble <- read_csv(dataFilePath, col_types = dataFileColTypes())

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "adding data for", columnName, "\n")
  }
  
  for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
    joinMe <- newStateDataTibble %>%
      select(Province_State, {aType}) %>%
      filter(!str_detect(Province_State, "Princess")) %>%
      mutate(Combined_Key = paste(Province_State, ", US", sep = ""),
             .keep = "unused", .before = .data[[aType]]) %>%
      mutate("{columnName}" := .data[[aType]],
             .keep = "unused", .after = "Combined_Key")
    
    newST[[aType]] <- left_join(newST[[aType]], joinMe,
                                by = "Combined_Key")
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving addDateToStateDataFilesForTypes\n")
  }
  
  return(newST)
}

addDateRangeToStateDataFilesForTypes <- function(newST, firstDate, lastDate,
                                                 traceThisRoutine = FALSE,
                                                 prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered addDateRangeToStateDataFilesForTypes\n")
  }
  
  dateDifference <- lastDate - firstDate
  
  for (i in 0:dateDifference) {
    newST <- addDateToStateDataFilesForTypes(newST, firstDate + i,
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving addDateRangeToStateDataFilesForTypes\n")
  }
  
  return(newST)
}

rebuildStateDataFilesForTypes <- function(nDates = 60, stopNDaysBeforePresent = 1,
                                          traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered rebuildStateDataFilesForTypes\n")
  }
  
  # First, let's be sure we have the dates in the format we need
  lastDate <- Sys.Date() - stopNDaysBeforePresent
  firstDate <- lastDate - nDates
  
  popFileName <- "US_Population.csv"
  commonColumns <- read_csv(paste(localDataDirectory(), popFileName, sep = ""),
                            col_types = populationColTypes()) %>%
    filter(Province_State == CountyName) %>%
    filter(Combined_Key != "US") %>%
    select(Combined_Key, Population) %>%
    filter(!str_detect(Combined_Key, "bia,Dis")) %>%
    filter(!str_detect(Combined_Key, "Princess")) %>%
    filter(!str_detect(Combined_Key, "Recovered"))
  
  emptyStateTibbles <- list(Total_Test_Results  = commonColumns,
                            Case_Fatality_Ratio = commonColumns,
                            Incident_Rate       = commonColumns,
                            Testing_Rate        = commonColumns)
  
  newStateTibbles <- addDateRangeToStateDataFilesForTypes(emptyStateTibbles, firstDate, lastDate,
                                                          traceThisRoutine = traceThisRoutine,
                                                          prepend = myPrepend)
  
  # for (i in 0:(nDates - 1)) {
  #   columnDate <- firstDate + i
  #   
  #   if (columnDate >= lastDate) {
  #     print(paste("Up to", as.character(Sys.Date()), "with i", i))
  #     break;
  #   }
  #   
  #   addDateToStateDataFilesForTypes(newST, columnDate,
  #                                               traceThisRoutine = traceThisRoutine,
  #                                               prepend = myPrepend)
  #   
  #   # dataFilePath <- dataPathForDate(columnDate)
  #   # columnName <- formatJHUDateForColumnName(jhuFileDateString(columnDate))
  #   # 
  #   # newStateDataTibble <- read_csv(dataFilePath, col_types = dataFileColTypes())
  #   # 
  #   # for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
  #   #   joinMe <- newStateDataTibble %>%
  #   #     select(Province_State, {aType}) %>%
  #   #     filter(!str_detect(Province_State, "Princess")) %>%
  #   #     mutate(Combined_Key = paste(Province_State, ", US", sep = ""),
  #   #            .keep = "unused", .before = .data[[aType]]) %>%
  #   #     mutate("{columnName}" := .data[[aType]],
  #   #            .keep = "unused", .after = "Combined_Key")
  #   #   
  #   #   newStateTibbles[[aType]] <- left_join(newStateTibbles[[aType]], joinMe,
  #   #                                         by = "Combined_Key")
  #   # }
  # }
  
  if (traceThisRoutine) {
    cxn <- file("./DEVELOPMENT/fileWriteLog.txt", "a")
  }
  
  for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
    # Save the results
    write_csv(newStateTibbles[[aType]], newStateDataPathForType(aType))
    if (traceThisRoutine) {
      cat(file = cxn, "Wrote", newStateDataPathForType(aType),
          "in rebuileStateDataFilesForTypes\n")
    }
  }
  
  if (traceThisRoutine) {
    close(cxn)
    cat(file = stderr(), prepend, "Leaving rebuildStateDataFilesForTypes\n")
  }
  
  return(newStateTibbles)
}

rebuildUSDataFileForTypeAsSummary <- function(stateDataTibble, aType,
                                              traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered rebuildUSDataFileForTypeAsSummary\n")
  }

  USDataFileName = newUSDataPathForType(aType)
  popFileName <- paste(localDataDirectory(), "US_Population.csv", sep="")
  
  buildingUSTibble <- read_csv(popFileName,
                                  col_types = populationColTypes()) %>%
    filter(Combined_Key == "US") %>%
    select(Combined_Key, Population)
  
  sansCombinedKeyTibble <- stateDataTibble %>%
    select(-Combined_Key)
  
  if ("Province_State" %in% names(sansCombinedKeyTibble)) {
    allNumTibble <- sansCombinedKeyTibble %>%
      select(-Province_State)
  } else {
    allNumTibble <- sansCombinedKeyTibble
  }

  newUSDataTibble <- allNumTibble %>%
    summarise(across(.cols = everything(), .fns = sum)) %>%
    mutate(Combined_Key = "US", .before = everything())

  write_csv(newUSDataTibble, USDataFileName)

  if (traceThisRoutine) {
    cxn <- file("./DEVELOPMENT/fileWriteLog.txt", "a")
    cat(file = cxn, "Wrote", USDataFileName,
        "in rebuildUSDataFileForTypeAsSummary\n")
    close(cxn)
    cat(file = stderr(), prepend, "Leaving rebuildUSDataFileForTypeAsSummary\n")
  }
  
  return(newUSDataTibble)
}

rebuildUSDataFileForTypeAsWeightedAvg <- function(stateTibble, aType,
                                                  traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered rebuildUSDataFileForTypeAsWeightedAvg\n")
    cat(file = stderr(), myPrepend, "aType =", aType, "\n")
  }
  
  # if "Population" is not a column of stateTibble, join it
  if (!("Population" %in% names(stateTibble))) {
    stateTibble <- right_join(US_State_Population_Est, stateTibble, by = "Combined_Key")
  }

  intermedUSTibble1 <- stateTibble %>%
    mutate(across(.cols = matches("^[1-9]+/"), ~(.x * Population)), .keep = "all")
  
  intermedUSTibble2 <- intermedUSTibble1 %>%
    filter(across(.cols = matches("^[1-9]+/"), ~(!is.na(.x)))) %>%
    summarize(Combined_Key = "US",
              across(.cols = (Population | matches("^[1-9]+/")), ~(sum(.x))))
  
  missingData <- intermedUSTibble1 %>%
    filter(across(.cols = matches("^[1-9]+/"), ~(is.na(.x))))

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "missingData:\n")
    cat(file = stderr(), myPrepend, "Combined_Key:", paste(missingData$Combined_Key, sep = ", "), "\n")
    cat(file = stderr(), myPrepend, "Population:  ", paste(missingData$Population, sep = ", "), "\n")
  }

  newUSTibble <- intermedUSTibble2 %>%
    mutate(across(.cols = matches("^[1-9]+/"), ~(.x / Population)))

  aType0 <- paste(aType, "_0", sep = "")
  write_csv(newUSTibble, newUSDataPathForType(aType0))

  if (traceThisRoutine) {
    cxn <- file("./DEVELOPMENT/fileWriteLog.txt", "a")
    cat(file = cxn, "Wrote", newUSDataPathForType(aType0),
        "in rebuildUSDataFileForTypeAsWeightedAvg\n")
    close(cxn)
    cat(file = stderr(), prepend, "Leaving rebuildUSDataFileForTypeAsWeightedAvg\n")
  }
  
  return(list(IM = intermedUSTibble2, FF = newUSTibble))
}

rebuildUSDataFileForTypeFromProperData <- function(USNumeratorTibble,
                                                   USDenominatorTibble,
                                                   aType,
                                                   traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered rebuildUSDataFileForTypeFromProperData\n")
  }
  
  commonColumns <- intersect(names(USNumeratorTibble), names(USDenominatorTibble))

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "aType is", aType, "\n")
    cat(file = stderr(), myPrepend, "commonColumns", paste(commonColumns, sep = ", "), "\n")
  }

  # Frankly, commonRows isn't a problem; there's only one row in the US data tibble anyway
  # But if we had more rows we would do this:
  #     commonRows <- intersect(USNumeratorTibble$Combined_Key, USDenominatorTibble$Combined_Key)
  #     aTibble %>%
  #       filter(Combined_Key %in% {commonRows})

  numData <- USNumeratorTibble %>%
    select(-Province_State, -Combined_Key) %>%
    select(any_of(commonColumns))
  
  denomData <- USDenominatorTibble %>%
    select(-Province_State, -Combined_Key) %>%
    select(any_of(commonColumns))

  KeyCol <- USDenominatorTibble %>%
    select(Combined_Key)
  
  Quotient <- numData / denomData
  
  newUSTibble <- bind_cols(KeyCol, Quotient)
 
  write_csv(newUSTibble, newUSDataPathForType(aType))

  if (traceThisRoutine) {
    cxn <- file("./DEVELOPMENT/fileWriteLog.txt", "a")
    cat(file = cxn, "Wrote", newUSDataPathForType(aType),
        "in rebuildUSDataFileForTypeFromProperData\n")
    close(cxn)
    cat(file = stderr(), prepend, "Leaving rebuildUSDataFileForTypeFromProperData\n")
  }
  
  return(newUSTibble)
}

rebuildUSDataFileForTypeByNormalizing <- function(USNumeratorTibble,
                                                   aType,
                                                  perHowMany = 100000,
                                                  traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered rebuildUSDataFileForTypeByNormalizing\n")
  }
  
  US_Population <- read_csv("./DATA/US_Population.csv", show_col_types = FALSE)
  
  popDivByHowMany <- as.double(US_Population %>%
                           filter(Combined_Key == "US") %>%
                           select(Population)) / perHowMany
  
  if (traceThisRoutine) {
  #   cat(file = stderr(), myPrepend, "names(USNumeratorTibble):",
  #       paste(names(USNumeratorTibble), sep = ", "), "\n")
    cat(file = stderr(), myPrepend, "popDivByHowMany =", round(popDivByHowMany, digits = 5), "\n")
  }

  UNTNonNum <- select(USNumeratorTibble, !matches("^[1-9]"))
  UNTNumeric <- select(USNumeratorTibble, matches("^[1-9]"))
  
  NormNumeric <- UNTNumeric / popDivByHowMany
  
  newUSTibble <- bind_cols(UNTNonNum, NormNumeric)
  
  write_csv(newUSTibble, newUSDataPathForType(aType))

  if (traceThisRoutine) {
    cxn <- file("./DEVELOPMENT/fileWriteLog.txt", "a")
    cat(file = cxn,
        "Wrote", newUSDataPathForType(aType),
        "in rebuildUSDataFileForTypeByNormalizing\n")
    close(cxn)
    cat(file = stderr(), prepend, "Leaving rebuildUSDataFileForTypeByNormalizing\n")
  }
  
  return(newUSTibble)
}


rebuildUSDataFilesForTypes <- function(stateTibbles, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered rebuildUSDataFilesForTypes\n")
  }
  
  US_Total_Test_Results <- rebuildUSDataFileForTypeAsSummary(stateTibbles$Total_Test_Results,
                                                             "Total_Test_Results",
                                                             traceThisRoutine = traceThisRoutine,
                                                             prepend = myPrepend)
  
  US_Deaths <- read_csv("./DATA/US_Deaths.csv",
                        col_types = myTSColTypes())
  US_Confirmed <- read_csv("./DATA/US_Confirmed.csv",
                           col_types = myTSColTypes())

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
  
  newUSTibbles <- list(US_TTR = US_Total_Test_Results,
                       US_CFR = US_Case_Fatality_Ratio,
                       US_IR = US_Incident_Rate,
                       US_TR = US_Testing_Rate)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving rebuildUSDataFilesForTypes\n")
  }
  
  return(newUSTibbles)
}

rebuildFourTypes <-  function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered rebuildFourTypes\n")
  }

  newStateTibbles <- rebuildStateDataFilesForTypes(traceThisRoutine = traceThisRoutine,
                                                   prepend = myPrepend)
  
  rebuildUSDataFilesForTypes(newStateTibbles,
                             traceThisRoutine = traceThisRoutine,
                             prepend = "")

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving rebuildFourTypes\n")
  }
}
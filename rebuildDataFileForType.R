# Rebuild US and State data files for data in daily files:
# Incident_Rate, Total_Test_Results, Case_Fatality_Ratio,
# Testing_Rate

# Daily update data is still available in ReopenAndRisk/DATA/mm-dd-2021.csv
# from 04-01-2021 through 07-21-2021

library(tidyverse)
library(RCurl)
library(glue)

source("./dateFormatRoutines.R")

theDataDirectory <- function() {
  "../ReopenAndRisk/DATA/"
}

localDataDirectory <- function() {
  "./DATA/"
}

newStateDataPathForType <- function(aType) {
  paste(localDataDirectory(), "US_State_", aType, ".csv", sep = "")
}

dataPathForDate <- function(aDate) {
  paste(theDataDirectory(), jhuFileDateString(aDate), ".csv",
        sep = "")
}

sumIgnoreNA <- function(x) {
  sum(x, na.rm = TRUE)
}

# We're going to need this to read a data file without
#  getting warnings
dataFileColSpec <- function() {
  cols(.default = col_double(),
       Province_State = col_character(),
       Country_Region = col_character(),
       Last_Update = col_datetime(format = ""),
       Recovered = col_logical(),           # all NAs so "double" doesn't work
       Active = col_logical(),              # ditto
       People_Hospitalized = col_logical(), # ditto
       ISO3 = col_character())
}

collectDateConstantColumns <- function(stateName, fileDate) {
  dataFilePath <- dataPathForDate(fileDate)
  newStateDataTibble <- read_csv(dataFilePath,
                                 col_types = dataFileColSpec()) %>% 
    select(Province_State, Country_Region, Lat, Long_, FIPS, UID, ISO3) %>%
    filter(Province_State == !!stateName)
}

collectConstantColumnsForState <- function(stateName) {
  nDates = 95
  firstDate <- Sys.Date() - nDates
  
  buildingConstDataTibble <- tibble(Province_State = NA, Country_Region = NA,
                                    Lat = NA, Long_ = NA, FIPS = NA,
                                    UID = NA, ISO3 = NA)
  
  for (i in 0:(nDates - 1)) { # 0:94
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

openExistingStateTibbles <- function() {
  existingStateTibbles <- list(Total_Test_Results  = NA,
                               Case_Fatality_Ratio = NA,
                               Incident_Rate       = NA,
                               Testing_Rate        = NA)
  for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
    existingStateTibbles[[aType]] <- read_csv(newStateDataPathForType(aType))
  }
  existingStateTibbles
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
  
  newStateDataTibble <- read_csv(dataFilePath, col_types = dataFileColSpec())

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
  # OUCH remove following for debugging:
  # traceThisRoutine <- TRUE

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
  # dataFileColSpec <- cols(.default = col_double(),
  #                         Province_State = col_character(),
  #                         Country_Region = col_character(),
  #                         Last_Update = col_datetime(format = ""),
  #                         Recovered = col_logical(),
  #                         Active = col_logical(),
  #                         People_Hospitalized = col_logical(),
  #                         ISO3 = col_character())

  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered rebuildStateDataFilesForTypes\n")
  }
  
  # First, let's be sure we have the dates in the format we need
  lastDate <- Sys.Date() - stopNDaysBeforePresent
  firstDate <- lastDate - nDates
  
  popFileName <- "US_Population.csv"
  commonColumns <- read_csv(paste(theDataDirectory(), popFileName, sep = ""),
                            col_types = cols(.default = col_character(),
                                             FIPS = col_integer(),
                                             Population = col_integer())) %>%
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
  #   # newStateDataTibble <- read_csv(dataFilePath, col_types = dataFileColSpec())
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
  
  for (aType in c("Total_Test_Results", "Case_Fatality_Ratio", "Incident_Rate", "Testing_Rate")) {
    # Save the results
    write_csv(newStateTibbles[[aType]], newStateDataPathForType(aType))
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving rebuildStateDataFilesForTypes\n")
  }
  
  return(newStateTibbles)
}

rebuildUSDataFileForTypeAsSummary <- function(stateDataTibble, aType) {
  # dataFileColSpec <- cols(.default = col_double(),
  #                         Province_State = col_character(),
  #                         Country_Region = col_character(),
  #                         Last_Update = col_datetime(format = ""),
  #                         Recovered = col_logical(),
  #                         Active = col_logical(),
  #                         People_Hospitalized = col_logical(),
  #                         ISO3 = col_character())

  USDataFileName = newStateDataPathForType(aType)
  popFileName <- paste(theDataDirectory(), "US_Population.csv", sep="")
  
  buildingUSTibble <- read_csv(popFileName,
                                  col_types = cols(.default = col_character(),
                                                   FIPS = col_integer(),
                                                   Population = col_integer())) %>%
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
    mutate(Combined_Key = "US", .before = Population)

  write_csv(newUSDataTibble, USDataFileName)
}

rebuildUSDataFileForTypeAsWeightedAvg <- function(stateTibble, aType) {
  newUSTibble <- stateTibble %>%
    mutate(across(.cols = matches("^[1-9]+/"), ~(.x * Population))) %>%
    filter(across(.cols = matches("^[1-9]+/"), ~(!is.na(.x)))) %>%
    summarize(Combined_Key = "US",
              across(.cols = (Population | matches("^[1-9]+/")), ~(sum(.x)))) %>%
    mutate(across(.cols = matches("^[1-9]+/"), ~(.x / Population)))

  aType0 <- paste(aType, "_0", sep = "")
  write_csv(newUSTibble, newStateDataPathForType(aType0))
  
  newUSTibble
}

rebuildUSDataFileForTypeFromProperData <- function(USNumeratorTibble,
                                                   USDenominatorTibble,
                                                   aType) {
  commonColumns <- intersect(names(USNumeratorTibble), names(USDenominatorTibble))
  
  # Frankly, commonRows isn't a problem; there's only one row in the US data tibble anyway
  # But if we had more rows we would do this:
  #     commonRows <- intersect(USNumeratorTibble$Combined_Key, USDenominatorTibble$Combined_Key)
  #     aTibble %>%
  #       filter(Combined_Key %in% {commonRows})

  numData <- USNumeratorTibble %>%
    select(any_of(commonColumns))
  
  denomData <- USDenominatorTibble %>%
    select(any_of(commonColumns))
  
  if ("Combined_Key" %in% commonColumns) {
    numData <-   numData %>%
      select(-Combined_Key)
    denomData <-   denomData %>%
      select(-Combined_Key)
  }
  if ("Population" %in% commonColumns) {
    numData <-   numData %>%
      select( -Population)
    denomData <-   denomData %>%
      select( -Population)
  }

  KeyPopCols <- USDenominatorTibble %>%
    select(Combined_Key, Population)
  
  Quotient <- numData / denomData
  
  newUSTibble <- bind_cols(KeyPopCols, Quotient)
 
  write_csv(newUSTibble, newStateDataPathForType(aType))
  
  newUSTibble
}

rebuildUSDataFileForTypeByNormalizing <- function(USNumeratorTibble,
                                                   aType) {
  popby100k <- as.double(US_Population %>%
                           filter(Combined_Key == "US") %>%
                           select(Population)) / 100000
  
  UNTNonNum <- select(USNumeratorTibble, !matches("^[1-9]"))
  UNTNumeric <- select(USNumeratorTibble, !matches("^[1-9]"))
  
  NormNumeric <- UNTNumeric / popby100k
  
  newUSTibble <- bind_cols(UNTNonNum, NormNumeric)
  
  write_csv(newUSTibble, newStateDataPathForType(aType))
  
  newUSTibble
}
  
rebuildFourTypes <-  function() {
  newStateTibbles <- rebuildStateDataFilesForTypes()
  # US_State_Total_Test_Results <- rebuildStateDataFileForType("Total_Test_Results")
  # US_State_Case_Fatality_Ratio <- rebuildStateDataFileForType("Case_Fatality_Ratio")
  # US_StateIncident_Rate <- rebuildStateDataFileForType("Incident_Rate")
  # US_StateTesting_Rate <- rebuildStateDataFileForType("Testing_Rate")
  
  US_Total_Test_Results <- rebuildUSDataFileForTypeAsSummary(newStateTibbles$Total_Test_Results,
                                                             "Total_Test_Results")

  US_Deaths <- read_csv("./DATA/US_Deaths.csv")
  US_Confirmed <- read_csv("./DATA/US_Confirmed.csv")

  # Hypothesis: Case_Fatality_Ratio <- Deaths / Confirmed
  US_Case_Fatality_Ratio_0 <- rebuildUSDataFileForTypeAsWeightedAvg(newStateTibbles$Case_Fatality_Ratio,
                                                                    "Case_Fatality_Ratio")
  
  # OUCH! US_Confirmed does not have a "population" column. Get it somehow before calling this.
  US_Case_Fatality_Ratio <- rebuildUSDataFileForTypeFromProperData(US_Deaths,
                                                                   US_Confirmed,
                                                                   "Case_Fatality_Ratio")
  
  # Hypothesis: Incident_Rate <- Confirmed / Population * 100000
  US_Incident_Rate_0 <- rebuildUSDataFileForTypeAsWeightedAvg(newStateTibbles$Incident_Rate,
                                                              "Incident_Rate")
  US_Incident_Rate <- rebuildUSDataFileForTypeByNormalizing(US_Confirmed,
                                                            "Incident_Rate")
  
  # Hypothesis: Testing_Rate <- Total_Test_Results / Population * 100000
  US_Testing_Rate_0 <- rebuildUSDataFileForTypeAsWeightedAvg(newStateTibbles$Testing_Rate,
                                                             "Testing_Rate")
  US_Testing_Rate <- rebuildUSDataFileForTypeByNormalizing(US_Total_Test_Results,
                                                           "Testing_Rate")
  
  list(USTTR = US_Total_Test_Results,
       US_CFR_0 = US_Case_Fatality_Ratio_0,
       US_CFR = US_Case_Fatality_Ratio,
       US_IR_0 = US_Incident_Rate_0,
       US_IR = US_Incident_Rate,
       US_TR_0 = US_Testing_Rate_0,
       US_TR = US_Testing_Rate)
}
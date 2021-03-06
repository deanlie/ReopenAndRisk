# Update for clean as you go branch

library(tidyverse)
library(lubridate)

inputDirectory <- function() {
  "./DATA/" # "DATA/"
}

outputDirectory <- function() {
  "./DATA/STATIC3/" # "./DATA/STATIC/"
}

inputPath <- function(aFileName) {
  paste(inputDirectory(), aFileName, sep = "")
}

outputPath <- function(aFileName) {
  paste(outputDirectory(), aFileName, sep = "")
}

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

getInputData <- function(aFileName) {
  inputData <- read_csv(inputPath(aFileName), show_col_types = FALSE)
}

writeOutputData <- function(outputData, aFileName) {
  write_csv(outputData, outputPath(aFileName))
}

copyDateRangeToDataStatic <- function(aFileName) {

  # Don't tell us about column spec problems
  inputData <- getInputData(aFileName)

  outputData <- discardDataOutsideDateRangeFromATibble(inputData, 
                                                       mdy("08/01/2021"),
                                                       mdy("09/20/2021"),
                                                       traceThisRoutine = TRUE)

  writeOutputData(outputData, aFileName)    
}

copyDataToDataStatic <- function(aFileName) {
  theCommand <- paste("cp", inputPath(aFileName), outputPath(aFileName), sep = "")
  cat(file = stderr(), theCommand, "\n")
  system(theCommand)
}

processOneFile <- function(aFileName) {
  cat(file = stderr(), "Processing", aFileName, "\n")
  copyDateRangeToDataStatic(aFileName)
}

USFileName <- function(aType) {
  paste("US_", aType, ".csv", sep = "")
}

StateFileName <- function(aType) {
  paste("US_State_", aType, ".csv", sep = "")
}

CountyFileName <- function(aType) {
  paste("US_County_", aType, ".csv", sep = "")
}

processUSFile <- function(aType) {
  inputData <- getInputData(USFileName(aType))
  
  outputData <- discardDataOutsideDateRangeFromATibble(inputData, 
                                                       mdy("11/08/21"),
                                                       mdy("11/23/2021"),
                                                       traceThisRoutine = TRUE)
  writeOutputData(outputData, USFileName(aType))
}

processStateFile <- function(aType) {
  inputData <- getInputData(StateFileName(aType))
  
  dateLimitedData <- discardDataOutsideDateRangeFromATibble(inputData, 
                                                            mdy("11/08/21"),
                                                            mdy("11/23/2021"),
                                                            traceThisRoutine = TRUE)
  outputData <- filter(dateLimitedData,
                       (Combined_Key == "Alabama, US") |
                         (Combined_Key == "Delaware, US") |
                         (Combined_Key == "Hawaii, US") |
                         (Combined_Key == "Ohio, US") |
                         (Combined_Key == "West Virginia, US"))

  writeOutputData(outputData, StateFileName(aType))
}

processCountyFile <- function(aType) {
  inputData <- getInputData(CountyFileName(aType))
  
  dateLimitedData <- discardDataOutsideDateRangeFromATibble(inputData, 
                                                       mdy("11/08/2021"),
                                                       mdy("11/23/2021"),
                                                       traceThisRoutine = TRUE)
  outputData <- filter(dateLimitedData,
                       (Combined_Key == "Autauga, Alabama, US") |
                         (Combined_Key == "Escambia, Alabama, US") |
                         (Combined_Key == "Kent, Delaware, US") |
                         (Combined_Key == "New Castle, Delaware, US") |
                         (Combined_Key == "Sussex, Delaware, US") |
                         (Combined_Key == "Honolulu, Hawaii, US") |
                         (Combined_Key == "Kalawao, Hawaii, US") |
                         (Combined_Key == "Maui, Hawaii, US") |
                         (Combined_Key == "Coshocton, Ohio, US") |
                         (Combined_Key == "Gallia, Ohio, US") |
                         (Combined_Key == "Cuyahoga, Ohio, US") |
                         (Combined_Key == "Greene, Ohio, US") |
                         (Combined_Key == "Boone, West Virginia, US") |
                         (Combined_Key == "Monongalia, West Virginia, US"))

  writeOutputData(outputData, CountyFileName(aType))
}

processAllFiles <- function() {
  for (aType in c("Vaccinations", "Testing_Rate", "Total_Test_Results", "Case_Fatality_Ratio",
                  "Incident_Rate")) {
    for (aLocale in c("US_", "US_State_")) {
      thePath <- paste(aLocale, aType, ".csv", sep = "")
      processOneFile(thePath)
    }
  }

  for (aType in c("Deaths", "Confirmed")) {
    for (aLocale in c("US_", "US_State_", "US_County_")) {
      thePath <- paste(aLocale, aType, ".csv", sep = "")
      processOneFile(thePath)
    }
  }
}

makeSmallSetOfStaticData <- function() {
  USStateCountyTypes <- c("Deaths", "Confirmed")
  USStateTypes <- c("Vaccinations", "Testing_Rate", "Total_Test_Results",
                    "Case_Fatality_Ratio", "Incident_Rate")
  
  for (aType in USStateTypes) {
    processUSFile(aType)
    processStateFile(aType)
  }
  for (aType in USStateCountyTypes) {
    processUSFile(aType)
    processStateFile(aType)
    processCountyFile(aType)
  }
}

redoCountyStaticData <- function() {
  USStateCountyTypes <- c("Deaths", "Confirmed")

  for (aType in USStateCountyTypes) {
    processCountyFile(aType)
  }
}

redoUSFiles <- function() {
  newST = list(Total_Test_Results = getInputData(StateFileName("Total_Test_Results")),
               Case_Fatality_Ratio = getInputData(StateFileName("Case_Fatality_Ratio")),
               Incident_Rate = getInputData(StateFileName("Incident_Rate")),
               Testing_Rate = getInputData(StateFileName("Testing_Rate")))

  newUST2 <- rebuildUSDataFilesForTypes(newST, traceThisRoutine = TRUE, prepend = "")
}

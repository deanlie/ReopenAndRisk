inputDirectory <- function() {
  "./DATA/" # "DATA/"
}

outputDirectory <- function() {
  "./DATA/STATIC2/" # "./DATA/STATIC/"
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
                                                       mdy("08/12/2021"),
                                                       mdy("09/20/2021"),
                                                       traceThisRoutine = TRUE)
  writeOutputData(outputData, USFileName(aType))
}

processStateFile <- function(aType) {
  inputData <- getInputData(StateFileName(aType))
  
  dateLimitedData <- discardDataOutsideDateRangeFromATibble(inputData, 
                                                            mdy("08/12/2021"),
                                                            mdy("09/20/2021"),
                                                            traceThisRoutine = TRUE)
  outputData <- filter(dateLimitedData,
                       (Combined_Key == "Alabama, US") |
                         (Combined_Key == "Louisiana, US") |
                         (Combined_Key == "Puerto Rico, US") |
                         (Combined_Key == "Massachusetts, US") |
                         (Combined_Key == "Maine, US") |
                         (Combined_Key == "Florida, US") |
                         (Combined_Key == "Texas, US"))

  writeOutputData(outputData, StateFileName(aType))
}

processCountyFile <- function(aType) {
  inputData <- getInputData(CountyFileName(aType))
  
  dateLimitedData <- discardDataOutsideDateRangeFromATibble(inputData, 
                                                       mdy("08/12/2021"),
                                                       mdy("09/20/2021"),
                                                       traceThisRoutine = TRUE)
  outputData <- filter(dateLimitedData,
                       (Combined_Key == "Autauga, Alabama, US") |
                         (Combined_Key == "Natchitoches, Louisiana, US") |
                         (Combined_Key == "Plaquemines, Louisiana, US") |
                         (Combined_Key == "Arecibo, Puerto Rico, US") |
                         (Combined_Key == "Barceloneta, Puerto Rico, US") |
                         (Combined_Key == "Barnstable, Massachusetts, US") |
                         (Combined_Key == "Berkshire, Massachusetts, US") |
                         (Combined_Key == "Dukes, Massachusetts, US") |
                         (Combined_Key == "Middlesex, Massachusetts, US") |
                         (Combined_Key == "Nantucket, Massachusetts, US") |
                         (Combined_Key == "Suffolk, Massachusetts, US") |
                         (Combined_Key == "Worcester, Massachusetts, US") |
                         (Combined_Key == "Penobscot, Maine, US") |
                         (Combined_Key == "Broward, Florida, US") |
                         (Combined_Key == "Brazoria, Texas, US") |
                         (Combined_Key == "Deaf Smith, Texas, US") |
                         (Combined_Key == "Harris, Texas, US"))

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
#  newUST <- rebuildUSDataFilesForTypes(newST, traceThisRoutine = TRUE, prepend = "")
  
  newUST2 <- rebuildUSDataFilesForTypes_B(newST, traceThisRoutine = TRUE, prepend = "")
  
  writeOutputData(newUST2$US_TTR, USFileName("Total_Test_Results"))
  writeOutputData(newUST2$US_CFR, USFileName("Case_Fatality_Ratio"))
  writeOutputData(newUST2$US_IR, USFileName("Incident_Rate"))
  writeOutputData(newUST2$US_TR, USFileName("Testing_Rate"))
  
}

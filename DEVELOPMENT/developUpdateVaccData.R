library(tidyverse)

source("downloadJHUData.R")
source("updateTimeSeriesDataFilesAsNecessary.R")

recreateVaccUpdateEnvironment <- function(staticDataQ) {
  # Remove ./DATA/US{,_State}_Vaccinations.csv
  if (staticDataQ) {
    system2("rm",
            args = c("./DATA/STATIC/US_State_Vaccinations.csv",
                     "./DATA/STATIC/US_Vaccinations.csv",
                     "./DATA/US_State_Vaccinations.csv",
                     "./DATA/US_Vaccinations.csv",
                     "./DATA/VaccTS_*.csv"))
    system2("cp",
            args = c("./DATA/STATIC/VariousEnds/US_*Vaccinations.csv",
                     "./DATA/STATIC/"))
  }
}

lastColName <- function(fileStemName,
                        traceThisRoutine = FALSE) {
  fileName <- paste("./DATA/STATIC/", fileStemName, ".csv",
                    sep = "")
  theTibble <- read_csv(fileName, show_col_types = FALSE)
  theNames <- names(theTibble)
  lastName <- theNames[length(theNames)]
  if (traceThisRoutine) {
    cat(file = stderr(), "last column of ", fileStemName,
        " is '", lastName, "'\n", sep = "")
  }
  return(lastName)
}

dataFileBaseNames <- function() {
  return(c("US_Confirmed",
           "US_State_Confirmed",
           "US_County_Confirmed",
           "US_Deaths",
           "US_State_Deaths",
           "US_County_Deaths",
           "US_Case_Fatality_Ratio",
           "US_State_Case_Fatality_Ratio",
           "US_Incident_Rate",
           "US_State_Incident_Rate",
           "US_Testing_Rate",
           "US_State_Testing_Rate",
           "US_Total_Test_Results",
           "US_State_Total_Test_Results",
           "US_Vaccinations",
           "US_State_Vaccinations"))
}

dataFilePaths <- function(fileBaseList, theDirectory) {
  result <- vector()
  for (fileBaseName in fileBaseList) {
    nextName <- paste(theDirectory, fileBaseName, ".csv", sep = "")
    result <- append(result, nextName)
  }
  return(result)
}

createTestEnvironmentTarFile <- function(fileBaseList, sourceDirectory, archiveDirectory) {
  thePaths <- dataFilePaths(fileBaseList, sourceDirectory)
  argumentList = c("cvf")
  argumentList = append(argumentList,
                        paste(archiveDirectory,
                              "UpdateTestData.tar",
                              sep = ""))
  argumentList = append(argumentList,
                        dataFilePaths(fileBaseList, sourceDirectory))
  
  # OUCH
  return(argumentList)

  system2("tar",
          c("cvf",
            "./DATA/UpdateTestData.tar",
            "./DATA/US_Confirmed.csv",
            "./DATA/US_State_Confirmed.csv",
            "./DATA/US_County_Confirmed.csv",
            "./DATA/US_Deaths.csv",
            "./DATA/US_State_Deaths.csv",
            "./DATA/US_County_Deaths.csv",
            "./DATA/US_Case_Fatality_Ratio.csv",
            "./DATA/US_State_Case_Fatality_Ratio.csv",
            "./DATA/US_Incident_Rate.csv",
            "./DATA/US_State_Incident_Rate.csv",
            "./DATA/US_Testing_Rate.csv",
            "./DATA/US_State_Testing_Rate.csv",
            "./DATA/US_Total_Test_Results.csv",
            "./DATA/US_State_Total_Test_Results.csv",
            "./DATA/US_Vaccinations.csv",
            "./DATA/US_State_Vaccinations.csv"))
}

restoreTestEnvironment <- function(staticDataQ = FALSE,
                                   traceThisRoutine = FALSE,
                                   prepend = "") {
  system2("tar",
          c("xvf",
            "./DATA/UpdateTestData.tar"))
}

filterResultsForTest <- function(fileBaseList,
                                 sourceDirectory,
                                 destDirectory = NULL,
                                 traceThisRoutine = FALSE,
                                 prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered filterResultsForTest\n")
  }
  
  for (fileBase in fileBaseList) {
    sourcePath <- paste(sourceDirectory, fileBase, ".csv", sep = "")
    destPath <- paste(destDirectory, fileBase, ".csv", sep = "")

    aTibble <- read_csv(sourcePath, show_col_types = FALSE)
    nCols <- dim(aTibble)[2]
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, sourcePath, "has", nCols, "columns\n")    
    }
    resTibble <- aTibble %>%
      select(1:2, last_col(5):last_col())
  }

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving filterResultsForTest\n")
  }
  return(resTibble)
}

evaluateResults <- function(staticDataQ = staticDataQ,
                            traceThisRoutine = traceThisRoutine,
                            prepend = myPrepend) {
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Enter evaluateResults\n")
    myPrepend <- paste("  ", prepend)
  }

  filterResultsForTest(c("US_Confirmed"),
                       "./DATA/STATIC/VariousEnds/",
                       traceThisRoutine = traceThisRoutine)
  
  result <- "INCONCLUSIVE"

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "PFFT!\n")
    cat(file = stderr(), prepend, "Leaving evaluateResults\n")
  }
  
  return(result)
}

testSuite <- function(staticDataQ = FALSE,
                      traceThisRoutine = FALSE,
                      prepend = "") {
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Enter testSuite\n")
    myPrepend <- paste("  ", prepend)
  }

  # restoreTestEnvironment(staticDataQ = staticDataQ,
  #                        traceThisRoutine = traceThisRoutine,
  #                        prepend = myPrepend)
  # 
  # loadAllUSData(staticDataQ = staticDataQ,
  #               traceThisRoutine = traceThisRoutine,
  #               prepend = myPrepend)

  result <- evaluateResults(staticDataQ = staticDataQ,
                            traceThisRoutine = traceThisRoutine,
                            prepend = myPrepend)
  
  cat(file = stderr(), myPrepend, "testSuite", result, "\n")

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testSuite\n")
  }
}

  

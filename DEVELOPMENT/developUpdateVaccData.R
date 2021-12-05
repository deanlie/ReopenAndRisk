library(tidyverse)

source("downloadJHUData.R")
source("updateTimeSeriesDataFilesAsNecessary.R")

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

vaccDataFileBaseNames <- function() {
  return(c("US_Vaccinations",
           "US_State_Vaccinations"))
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

createTestEnvironmentTarFile <- function(fileBaseList,
                                         sourceDirectory,
                                         archiveDirectory,
                                         archiveBaseName) {
  argumentList = c("cvf")
  argumentList = append(argumentList,
                        paste(archiveDirectory,
                              archiveBaseName,
                              ".tar",
                              sep = ""))
  argumentList = append(argumentList,
                        dataFilePaths(fileBaseList, sourceDirectory))
  
  system2("tar", argumentList)
}

restoreTestEnvironment <- function(staticDataQ = FALSE,
                                   traceThisRoutine = FALSE,
                                   prepend = "") {
  system2("rm",
          c("VaccTS*"))
  system2("tar",
          c("xvf",
            "./DATA/ClipDates/TarFiles/UpdateTestData.tar"))
}

restoreVaccFileTestEnvironment <- function(staticDataQ = FALSE,
                                           traceThisRoutine = FALSE,
                                           prepend = "") {
  system2("rm",
          c("./DATA/VaccTS*"))
  system2("tar",
          c("xvf",
            "./DATA/ClipDates/TarFiles/UpdateVaccTestData.tar"))
}

filterATibble <- function(aTibble,
                          traceThisRoutine = FALSE,
                          prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered filterATibble\n")
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "input tibble has",
        dim(aTibble)[2], "columns\n")    
  }

  resTibble <- aTibble %>%
    select(Combined_Key, last_col(5):last_col()) %>%
    filter((Combined_Key == "Autauga, Alabama, US") |
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
             (Combined_Key == "Monongalia, West Virginia, US") |
             (Combined_Key == "Alabama, US") |
             (Combined_Key == "Delaware, US") |
             (Combined_Key == "Hawaii, US") |
             (Combined_Key == "Ohio, US") |
             (Combined_Key == "West Virginia, US") |
             (Combined_Key == "US"))
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Filtered tibble has",
          dim(resTibble)[2], "columns and", dim(resTibble)[1], "row(s)\n")
  }
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving filterATibble\n")
  }
  return(resTibble)
}

saveFilteredFile <- function(resTibble,
                             fileBaseName,
                             destDirectory,
                             traceThisRoutine = FALSE,
                             prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered saveFilteredFile\n")
  }

  destPath <- paste(destDirectory, fileBaseName, ".csv", sep = "")
  write_csv(resTibble, destPath)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Saved", destPath, "\n")    
  }
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving saveFilteredFile\n")
  }
  
}

checkFilteredFile <- function(resTibble,
                              fileBaseName,
                              referenceDirectory,
                              traceThisRoutine = FALSE,
                              prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend,
        "Entered checkFilteredFile", fileBaseName, "\n")
  }

  maxNFails <- 5
  nFails <- 0  
  referencePath <- paste(referenceDirectory, fileBaseName, ".csv", sep = "")
  referenceTibblePlus <- read_csv(referencePath, show_col_types = FALSE)
  referenceTibble <- filterATibble(referenceTibblePlus,
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
  if ((dim(referenceTibble)[1] == dim(resTibble)[1]) &&
      (dim(referenceTibble)[2] == dim(resTibble)[2])) {
    for (i in 1:dim(referenceTibble)[1]) { # for each row
      if (referenceTibble[i, 1] != resTibble[i, 1]) {
        if ((nFails < maxNFails) && (traceThisRoutine)) {
          cat(file = stderr(), myPrepend,
              " CombinedKey[", i, "] was ", resTibble[i, 1],
              " expected ", referenceTibble[i, 1], sep = "")
        }
        nFails <- nFails + 1
      }
      for (j in 2:dim(referenceTibble)[2]) {
        if (!is.na(referenceTibble[i, j]) && !is.na(resTibble[i, j])) {
          if ((as.double(referenceTibble[i, j]) != as.double(resTibble[i, j]))) {
            if ((nFails < maxNFails) && (traceThisRoutine)) {
              cat(file = stderr(), myPrepend,
                  " resTibble[", i,", ", j, "] was ", as.double(resTibble[i, j]),
                  " expected ", as.double(referenceTibble[i, j]), "\n", sep = "")
            }
            nFails <- nFails + 1
          }
        } else {
          if ((is.na(referenceTibble[i, j]) && !is.na(resTibble[i, j])) ||
              (!is.na(referenceTibble[i, j]) && is.na(resTibble[i, j]))) {
            nFails <- nFails + 1
            if ((nFails < maxNFails) && (traceThisRoutine)) {
              cat(file = stderr(), myPrepend,
                  " NA vs value at resTibble[", i,", ", j, "]\n", sep = "")
            }
          }
        }
      }
    }
  } else {
    nFails <- nFails + 1
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend,
          " Array size mismatch, ",
          "resTibble[", dim(resTibble)[1], ", ", dim(resTibble)[2], "], ",
          "referenceTibble[", dim(referenceTibble)[1], ", ",
          dim(referenceTibble)[2], "]\n", sep = "")
    }
  }

  if (traceThisRoutine) {
    if (nFails == 0) {
      cat(file = stderr(), myPrepend, "comparison succeeds.\n")
    } else {
      cat(file = stderr(), myPrepend, "comparison has", nFails, "failures.\n")    
    }
  }
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving checkFilteredFile\n")
  }
  
  return(nFails)
}

filterMaybeSaveOrCheckFile <- function(fileBaseName,
                                       sourceDirectory,
                                       destDirectory = NULL,
                                       referenceDirectory = NULL,
                                       traceThisRoutine = FALSE,
                                       prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered filterMaybeSaveOrCheckFile\n")
  }
  
  sourcePath <- paste(sourceDirectory, fileBaseName, ".csv", sep = "")
  
  aTibble <- read_csv(sourcePath, show_col_types = FALSE)
  resTibble <- filterATibble(aTibble,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
  nFails <- 0

  if (!is.null(destDirectory)) {
    saveFilteredFile(resTibble,
                     fileBaseName,
                     destDirectory,
                     traceThisRoutine,
                     myPrepend)
  }
  if (!is.null(referenceDirectory)) {
    nFails <- checkFilteredFile(resTibble,
                                fileBaseName,
                                referenceDirectory,
                                traceThisRoutine,
                                myPrepend)
  }
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), myPrepend, "result =", nFails, "\n")
    cat(file = stderr(), prepend, "Leaving filterMaybeSaveOrCheckFile\n")
  }
  
  return(nFails)
}
  
filterMaybeSaveOrCheck <- function(fileBaseList,
                                   sourceDirectory,
                                   destDirectory = NULL,
                                   referenceDirectory = NULL,
                                   traceThisRoutine = FALSE,
                                   prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered filterMaybeSaveOrCheck\n")
  }
  
  nFails <- 0
  
  for (fileBase in fileBaseList) {
    nFileFails <- filterMaybeSaveOrCheckFile(fileBase,
                                             sourceDirectory,
                                             destDirectory,
                                             referenceDirectory,
                                             traceThisRoutine,
                                             myPrepend)
    
    nFails <- nFails + nFileFails
    if (nFileFails > 0) {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, fileBase, "FAILS\n")
      }
    } else {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, fileBase, "passes\n")
      }
    }
  }

  if (traceFlagOnEntry) {
    cat(file = stderr(), myPrepend, nFails, "failure(s)\n")
    cat(file = stderr(), prepend, "Leaving filterMaybeSaveOrCheck\n")
  }

  return(nFails)
}

evaluateResults <- function(staticDataQ = staticDataQ,
                            traceThisRoutine = FALSE,
                            prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Enter evaluateResults\n")
  }

  nFails <- filterMaybeSaveOrCheck(c("US_Vaccinations",
                                     "US_State_Vaccinations"),
                                   "./DATA/", # Source
                                   NULL,      # Dest
                                   "./DATA/REFERENCE/",      # Ref
                                   traceThisRoutine = TRUE,
                                   prepend = myPrepend)
  
  if (nFails == 0) {
    result <- "PASS"
  } else {
    if (nFails < 0) {
      result <- "INCONCLUSIVE"
    } else {
      result <- "FAIL"
    }
  }  

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "nFails was", nFails, "returning", result, "\n")
    cat(file = stderr(), prepend, "Leaving evaluateResults\n")
  }
  
  return(result)
}

howIMadeUpdateTestDataDotTar <- function() {
  createTestEnvironmentTarFile(dataFileBaseNames(),
                               "./DATA/",
                               "./DATA/ClipDates/TarFiles/",
                               "UpdateTestData")
}

howIMadeUpdateVaccTestDataDotTar <- function() {
  createTestEnvironmentTarFile(vaccDataFileBaseNames(),
                               "./DATA/",
                               "./DATA/ClipDates/TarFiles/",
                               "UpdateVaccTestData")
}

# Test routine for comparison / result evaluation routine
mungeAndTest <- function(fileBaseName,
                         sourceDirectory,
                         traceThisRoutine = FALSE,
                         prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered mungeAndTest\n")
  }

  # Read named file
  sourceTibble <- read_csv(paste(sourceDirectory, fileBaseName, ".csv", sep = ""),
                           show_col_types = FALSE)
  # Filter it
  resTibble <- filterATibble(sourceTibble,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
  # Modify one or more values
  resTibble[1, 6] <- as.double(resTibble[1,6]) - 5.0
  resTibble[1, 4] <- NA
  nExpectedFails <- 2
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "munged res to create",
        nExpectedFails, "failures\n")
  }
  
  nFails <- checkFilteredFile(resTibble,
                              fileBaseName,
                              sourceDirectory,
                              traceThisRoutine = traceThisRoutine,
                              prepend = myPrepend)

  if (traceThisRoutine) {
    if (nFails == nExpectedFails) {
      cat(file = stderr(), myPrepend,
          "The expected failures were detected properly\n")
    } else {
      cat(file = stderr(), myPrepend,
          "PROBLEM with checkFilteredFile, expected", nExpectedFails,
          "failures, detected", nFails, "\n")
    }
  }
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving mungeAndTest\n")
  }

  return(nFails)
}

thisFails <- function() {
  foo4 <- filterMaybeSaveOrCheckFile("US_State_Vaccinations",
                                     "./DATA/",
                                     NULL,
                                     "./DATA/STATIC3/",
                                     traceThisRoutine = TRUE,
                                     prepend = "")
}

testSuite <- function(staticDataQ = FALSE,
                      traceThisRoutine = FALSE,
                      prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Enter testSuite\n")
  }

  # Test for the file comparison routine. It passed.
  # mungeAndTest("US_State_Vaccinations", "./DATA/",
  #              traceThisRoutine = traceThisRoutine,
  #              prepend = myPrepend)

  # OUCH these two calls are what we really want:
  # restoreTestEnvironment(staticDataQ = staticDataQ,
  #                        traceThisRoutine = traceThisRoutine,
  #                        prepend = myPrepend)
  # 
  # loadAllUSData(staticDataQ = staticDataQ,
  #               traceThisRoutine = traceThisRoutine,
  #               prepend = myPrepend)
  
  restoreVaccFileTestEnvironment(staticDataQ = staticDataQ,
                         traceThisRoutine = traceThisRoutine,
                         prepend = myPrepend)
  updateDataFilesForUSVaccTimeSeriesIfNeeded(traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)
    
  result <- evaluateResults(staticDataQ = staticDataQ,
                            traceThisRoutine = traceThisRoutine,
                            prepend = myPrepend)
  
  cat(file = stderr(), myPrepend, "testSuite", result, "\n")

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testSuite\n")
  }
}

  

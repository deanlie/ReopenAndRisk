library(tidyverse)

source("downloadJHUData.R")
source("updateTimeSeriesDataFilesAsNecessary.R")
source("loadAllUSData.R")
source("testFileReconstruction.R")

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

dataFilePaths <- function(fileBaseList, theDirectory) {
  result <- vector()
  for (fileBaseName in fileBaseList) {
    nextName <- paste(theDirectory, fileBaseName, ".csv", sep = "")
    result <- append(result, nextName)
  }
  return(result)
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

discardDataOutsideDateRangeFromAFile <- function(thePath,
                                                 firstDateToKeep,
                                                 lastDateToKeep,
                                                 traceThisRoutine = FALSE,
                                                 prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered discardDataOutsideDateRangeFrom", thePath, "\n")
  }

  originalData <- read_csv(thePath, show_col_types = FALSE)

  truncatedTibble <- discardDataOutsideDateRangeFromATibble(originalData,
                                                            firstDateToKeep,
                                                            lastDateToKeep,
                                                            traceThisRoutine = traceThisRoutine,
                                                            prepend = myPrepend)

  write_csv(truncatedTibble, thePath)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving discardDataOutsideDateRangeFromFile\n")
  }

  return(truncatedTibble)
}

createTestEnvironmentTarFile <- function(fileBaseList,
                                         sourceDirectory = "./DATA/",
                                         archiveDirectory = "./DATA/ClipDates/TarFiles/",
                                         archiveBaseName = "UpdateTestData") {
  # 1. Clip all files in list to date range.
  #       Later, clip one at a time more than the rest
  #       to be sure it gets fully updated.
  for (aPath in dataFilePaths(fileBaseList, sourceDirectory)) {
    discardDataOutsideDateRangeFromAFile(aPath,
                                         today("EST") - 37,
                                         today("EST") - 2)
  }
  
  # 2. Create tar file
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
          c("./DATA/12*2021.csv"))
  system2("tar",
          c("xvf",
            "./DATA/ClipDates/TarFiles/UpdateTestData.tar"))
}

restoreVaccFileTestEnvironment <- function(staticDataQ = FALSE,
                                           traceThisRoutine = FALSE,
                                           prepend = "") {
  system2("rm",
          c("./DATA/VaccTS*", "./DATA/12*2021.csv"))
  system2("tar",
          c("xvf",
            "./DATA/ClipDates/TarFiles/UpdateVaccTestData.tar"))
}


saveFilteredFile <- function(resultTibble,
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
  write_csv(resultTibble, destPath)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Saved", destPath, "\n")    
  }
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving saveFilteredFile\n")
  }
}


compareFileWithReference <- function(fileBaseName,
                                     resultDirectory = "./DATA/",
                                     referenceDirectory = "./DATA/REFERENCE/",
                                     traceThisRoutine = FALSE,
                                     prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend,
        "Entered compareFileWithReference", fileBaseName, "\n")
  }
  
  resultPath <- paste(resultDirectory, fileBaseName, ".csv", sep = "")
  resultTibble <- read_csv(resultPath, show_col_types = FALSE)
  
  referencePath <- paste(referenceDirectory, fileBaseName, ".csv", sep = "")
  referenceTibble <- read_csv(referencePath, show_col_types = FALSE)
  
  nFails <- compareTibbleWithReference(resultTibble,
                                       referenceTibble,
                                       fileBaseName,
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving compareFileWithReference\n")
  }
  
  return(nFails)
}

getFilteredTibbleFromRawFile <- function(fileBaseName,
                                         sourceDirectory,
                                         traceThisRoutine = FALSE,
                                         prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend,
        "Entered getFilteredTibbleFromRawFile", fileBaseName, "\n")
  }

  thePath <- paste(sourceDirectory, fileBaseName, ".csv", sep = "")
  theTibblePlus <- read_csv(thePath, show_col_types = FALSE)
  theTibble <- filterATibble(theTibblePlus,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving getFilteredTibbleFromRawFile\n")
  }
  
  return(theTibble)
}

checkFilteredFile <- function(resultTibble,
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
  
  referenceTibble <- getFilteredTibbleFromRawFile(fileBaseName,
                                                  referenceDirectory,
                                                  traceThisRoutine = traceThisRoutine,
                                                  prepend = myPrepend)

  nFails <- compareTibbleWithReference(resultTibble,
                                       referenceTibble,
                                       fileBaseName,
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving checkFilteredFile\n")
  }
  
  return(nFails)
}

checkOne <- function(fileBaseName,
                     resultDirectory = "./DATA/",
                     referenceDirectory = "./DATA/REFERENCE/",
                     filterThem = TRUE,
                     traceThisRoutine = TRUE) {
  if(traceThisRoutine) {
    cat(file = stderr(), "Checking", fileBaseName, "\n")
  }
  resultPath <- paste(resultDirectory, fileBaseName, ".csv", sep = "")
  refPath <- paste(referenceDirectory, fileBaseName, ".csv", sep = "")
  
  sourceTibble <- read_csv(resultPath, show_col_types = FALSE)
  refTibble <- read_csv(refPath, show_col_types = FALSE)
  
  if (filterThem) {
    filteredSourceTibble <- filterATibble(sourceTibble,
                                          traceThisRoutine = traceThisRoutine,
                                          prepend = "  ")
    filteredRefTibble <- filterATibble(refTibble,
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = "  ")
    combinedTibble <- bind_rows(filteredRefTibble, filteredSourceTibble)
  } else {
    combinedTibble <- bind_rows(refTibble, sourceTibble)
  }
  
  View(combinedTibble)
  x <- 1
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
    cat(file = stderr(), prepend,
        "Entered filterMaybeSaveOrCheckFile for",
        fileBaseName, "\n")
  }
  traceThisRoutine <- FALSE
  
  sourcePath <- paste(sourceDirectory, fileBaseName, ".csv", sep = "")
  
  aTibble <- read_csv(sourcePath, show_col_types = FALSE)
  resultTibble <- filterATibble(aTibble,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
  nFails <- 0

  if (!is.null(destDirectory)) {
    saveFilteredFile(resultTibble,
                     fileBaseName,
                     destDirectory,
                     traceThisRoutine,
                     myPrepend)
  }
  if (!is.null(referenceDirectory)) {
    nFails <- checkFilteredFile(resultTibble,
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
  traceThisRoutine <- FALSE
  
  nFails <- 0
  
  for (fileBase in fileBaseList) {
    nFileFails <- filterMaybeSaveOrCheckFile(fileBase,
                                             sourceDirectory,
                                             destDirectory,
                                             referenceDirectory,
                                             traceThisRoutine,
                                             myPrepend)
    
    nFails <- nFails + nFileFails
    if (traceFlagOnEntry) {
      cat(file = stderr(), myPrepend, fileBase)
      if (nFileFails > 0) {
        cat(file = stderr(), "FAILS at", nFileFails, "locations \n")
      } else {
        cat(file = stderr(), "passes\n")
      }
    }
  }

  if (traceFlagOnEntry) {
    cat(file = stderr(), myPrepend, nFails, "failure(s) in total\n")
    cat(file = stderr(), prepend, "Leaving filterMaybeSaveOrCheck\n")
  }

  return(nFails)
}

evaluateFileResultAcrossProjects <- function(fileBaseName,
                                             resultDirectory,
                                             referenceDirectory,
                                             doFilter = TRUE,
                                             maxNFails = 5,
                                             traceThisRoutine = FALSE,
                                             prepend = "")
{
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Enter evaluateFileResultAcrossProjects\n")
  }

  resultPath <- paste(resultDirectory, fileBaseName, ".csv", sep = "")
  refPath <- paste(referenceDirectory, fileBaseName, ".csv", sep = "")
  
  resultTibble <- read_csv(resultPath, show_col_types = FALSE)
  refTibble <- read_csv(refPath, show_col_types = FALSE)
  
  if (doFilter) {
    resultTibble <- filterATibble(resultTibble,
                                  traceThisRoutine = traceThisRoutine,
                                  prepend = myPrepend)
    refTibble <- filterATibble(refTibble,
                               traceThisRoutine = traceThisRoutine,
                               prepend = myPrepend)
  }

  nFails <- compareTibbleWithReference(resultTibble,
                                       refTibble,
                                       fileBaseName,
                                       maxNFails = maxNFails,
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "nFails was", nFails, "\n")
    cat(file = stderr(), prepend, "Leaving evaluateFileResultAcrossProjects\n")
  }
  
  return(nFails)
}

evaluateFileListResultsAcrossProjects <- function(fileBaseList,
                                                  sourceDir,
                                                  referenceDir,
                                                  doFilter = TRUE,
                                                  maxNFails = 5,
                                                  traceEachFile = FALSE,
                                                  traceThisRoutine = FALSE,
                                                  prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend,
        "Enter evaluateFileListResultsAcrossProjects\n")
  }

  nFails <- 0
  
  for (fileBase in fileBaseList) {
    nFileFails <- evaluateFileResultAcrossProjects(fileBase,
                                                   sourceDir,
                                                   referenceDir,
                                                   doFilter = doFilter,
                                                   maxNFails = maxNFails,
                                                   traceThisRoutine = traceEachFile,
                                                   prepend = myPrepend)
    
    nFails <- nFails + nFileFails
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, fileBase)
      if (nFileFails > 0) {
        cat(file = stderr(), " FAILS at", nFileFails, "location(s) \n")
      } else {
        cat(file = stderr(), " passes\n")
      }
    }
  }

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "nFails was", nFails, "\n")
    cat(file = stderr(), prepend, "Leaving evaluateFileListResultsAcrossProjects\n")
  }
  
  return(nFails)
}

evaluateResults <- function(firstInDay = FALSE,
                            staticDataQ = FALSE,
                            traceThisRoutine = FALSE,
                            prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Enter evaluateResults\n")
  }
  
  if (firstInDay) {
    destDir <- "./DATA/REFERENCE/"
    refDir <- NULL
  } else {
    destDir <- NULL
    refDir <- "./DATA/REFERENCE/"
  }

  nFails <- filterMaybeSaveOrCheck(dataFileBaseNames(),
                                   "./DATA/", # Source
                                   destDir, 
                                   refDir,
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
  resultTibble <- filterATibble(sourceTibble,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
  # Modify one or more values
  resultTibble[1, 6] <- as.double(resultTibble[1,6]) - 5.0
  resultTibble[1, 4] <- NA
  nExpectedFails <- 2
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "munged res to create",
        nExpectedFails, "failures\n")
  }
  
  nFails <- checkFilteredFile(resultTibble,
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

# 
coreOfUpdateRoutine <- function(mismatches) {
  # OUCH
  while (dim(mismatches)[1] > 0) {
    firstDate <- mismatches$lastUpdate[1] + 1
    firstGroup <- filter(mismatches, lastUpdate <= {{firstDate}})
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, dim(firstGroup)[1],
          "files only updated to", firstDate, "\n")
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
    for (aDate in (firstDate:nextDate)) {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "Updating to", aDate, "\n")
      }
      # OUCH call the real update routine here! It can be a parameter
      #   of this routine.
    }
    mismatches <- remainingMismatches
  }
}

test1 <- function() {
  sourceDir <- "../RefactorAndRisk/DATA/"
  referenceDir <- "../ReopenAndRisk/DATA/"

  fileBaseList1 <- dataFileBaseNames()
  evaluateFileListResultsAcrossProjects(fileBaseList1,
                                        sourceDir,
                                        referenceDir,
                                        doFilter = TRUE,
                                        maxNFails = 5,
                                        traceEachFile = FALSE,
                                        traceThisRoutine = TRUE,
                                        prepend = "")

  problemFileBaseList = c("US_Confirmed",
                          "US_Deaths",
                          "US_Case_Fatality_Ratio",
                          "US_State_Case_Fatality_Ratio",
                          "US_Incident_Rate",
                          "US_State_Incident_Rate")
  evaluateFileListResultsAcrossProjects(problemFileBaseList,
                                        sourceDir,
                                        referenceDir,
                                        doFilter = FALSE,
                                        maxNFails = 10,
                                        traceEachFile = TRUE,
                                        traceThisRoutine = TRUE,
                                        prepend = "")
}

testSuite <- function(firstInDay = FALSE,
                      staticDataQ = FALSE,
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

  restoreTestEnvironment(staticDataQ = staticDataQ,
                         traceThisRoutine = traceThisRoutine,
                         prepend = myPrepend)

  updateDataFilesForUSVaccTimeSeriesIfNeeded(staticDataQ,
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)

  updateTimeSeriesDataFilesAsNecessary(staticDataQ,
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)

  updateSerializedDataFilesAsNecessary(staticDataQ,
                                       traceThisRoutine = traceThisRoutine,
                                       prepend = myPrepend)

  result <- evaluateResults(firstInDay = firstInDay,
                            staticDataQ = staticDataQ,
                            traceThisRoutine = traceThisRoutine,
                            prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testSuite\n")
  }
}

library(tidyverse)

# dataFileBaseNames()
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
  
  resultTibble <- aTibble %>%
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
        dim(resultTibble)[2], "columns and", dim(resultTibble)[1], "row(s)\n")
  }
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving filterATibble\n")
  }
  return(resultTibble)
}

compareTibbleWithReference <- function(resultTibble,
                                       referenceTibble,
                                       fileBaseName,
                                       maxNFails = 5,
                                       traceThisRoutine = FALSE,
                                       prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend,
        "Entered compareTibbleWithReference", fileBaseName, "\n")
  }
  nFails <- 0
  nNameFails <- 0
  nKeyFails <- 0
  if ((dim(referenceTibble)[1] == dim(resultTibble)[1]) &&
      (dim(referenceTibble)[2] == dim(resultTibble)[2])) {
    resultNames <- names(resultTibble)
    referenceNames <- names(referenceTibble)
    
    for (i in 1:dim(referenceTibble)[2]) { # for each column
      if (resultNames[i] != referenceNames[i]) {
        if ((nNameFails < maxNFails) && (traceThisRoutine)) {
          cat(file = stderr(), myPrepend,
              " resultTibble name[", i, "] was ", resultNames[i],
              " expected ", referenceNames[i], "\n", sep = "")
        }
        nNameFails <- nNameFails + 1
        nFails <- nFails + 1
      }
    }
    if (nNameFails == 0) {
      referenceNonNum <- select(referenceTibble, !matches("^[1-9]"))
      referenceNumeric <- select(referenceTibble, matches("^[1-9]"))
      resultNonNum <- select(resultTibble, !matches("^[1-9]"))
      resultNumeric <- select(resultTibble, matches("^[1-9]"))
      for (i in 1:dim(referenceNonNum)[1]) { # for each row
        keyFails <- 0
        for (j in 1:dim(referenceNonNum)[2]) { # for each character column
          if ((!is.na(referenceNonNum[i,j])) &&
              (!is.na(resultNonNum[i,j])) &&
              (referenceNonNum[i, j] != resultNonNum[i, j])) {
            if ((nKeyFails < maxNFails) && (traceThisRoutine)) {
              cat(file = stderr(), myPrepend,
                  " result name[", i, "] was ",
                  as.character(resultNonNum[i, j]),
                  " expected ",
                  as.character(referenceNonNum[i, j]), "\n", sep = "")
            }
            nFails <- nFails + 1
            keyFails <- 1
            nKeyFails <- nKeyFails + 1
          } else {
            if ((is.na(referenceNonNum[i, j]) && !is.na(resultNonNum[i, j])) ||
                (!is.na(referenceNonNum[i, j]) && is.na(resultNonNum[i, j]))) {
              if ((keyFails == 0) && (nKeyFails < maxNFails) && traceThisRoutine) {
                cat(file = stderr(), myPrepend,
                    " result name[", i, "] was ",
                    as.character(resultNonNum[i, j]),
                    " expected ",
                    as.character(referenceNonNum[i, j]), "\n", sep = "")
                keyFails <- 1
                nKeyFails <- nKeyFails + 1
              }
            }
          }
        }
        for (j in 1:dim(referenceNumeric)[2]) { # for each numeric column
          if (!is.na(referenceNumeric[i, j]) && !is.na(resultNumeric[i, j])) {
            if ((as.double(referenceNumeric[i, j]) != as.double(resultNumeric[i, j]))) {
              if ((keyFails == 0) && (nKeyFails < maxNFails) && traceThisRoutine) {
                cat(file = stderr(), myPrepend,
                    " Combined_Key[", i, "] was ", resultNonNum$Combined_Key[i],
                    " resultNumeric[", i,", ", j, "] was ", as.double(resultNumeric[i, j]),
                    " expected ", as.double(referenceNumeric[i, j]), "\n", sep = "")
                keyFails <- 1
                nKeyFails <- nKeyFails + 1
              } else {
                if ((nFails < maxNFails) && traceThisRoutine) {
                  cat(file = stderr(), myPrepend,
                      "   resultNumeric[", i,", ", j, "] was ", as.double(resultNumeric[i, j]),
                      " expected ", as.double(referenceNumeric[i, j]), "\n", sep = "")
                }
              }
              nFails <- nFails + 1
            }
          } else {
            if ((is.na(referenceNumeric[i, j]) && !is.na(resultNumeric[i, j])) ||
                (!is.na(referenceNumeric[i, j]) && is.na(resultNumeric[i, j]))) {
              if ((keyFails == 0) && (nKeyFails < maxNFails) && traceThisRoutine) {
                cat(file = stderr(), myPrepend,
                    " Combined_Key[", i, "] was ", resultNonNum$Combined_Key[i],
                    " NA vs value at resultNumeric[", i,", ", j, "]\n", sep = "")
                keyFails <- 1
                nKeyFails <- nKeyFails + 1
              } else {
                if ((nFails < maxNFails) && (traceThisRoutine)) {
                  cat(file = stderr(), myPrepend,
                      " NA vs value at resultNonNum[", i,", ", j, "]\n", sep = "")
                }
              }
              nFails <- nFails + 1
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
          "resultTibble[", dim(resultTibble)[1], ", ",
          dim(resultTibble)[2], "], ",
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
    cat(file = stderr(), prepend, "Leaving compareTibbleWithReference\n")
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
  failingBases <- vector()
  
  for (fileBase in fileBaseList) {
    nFileFails <- evaluateFileResultAcrossProjects(fileBase,
                                                   sourceDir,
                                                   referenceDir,
                                                   doFilter = doFilter,
                                                   maxNFails = maxNFails,
                                                   traceThisRoutine = traceEachFile,
                                                   prepend = myPrepend)
    
    nFails <- nFails + nFileFails
    if (nFileFails > 0) {
      failingBases <- append(failingBases, fileBase)
    }
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
  
  return(failingBases)
}

testFileReconstruction <- function() {
  sourceDir <- "../RefactorAndRisk/DATA/"
  referenceDir <- "../ReopenAndRisk/DATA/"

  problemFileBaseList = c("US_State_Case_Fatality_Ratio",
                          "US_State_Incident_Rate",
                          "US_State_Testing_Rate",
                          "US_State_Total_Test_Results")
  
  # evaluateFileListResultsAcrossProjects(problemFileBaseList,
  #                                       sourceDir,
  #                                       referenceDir,
  #                                       doFilter = TRUE,
  #                                       maxNFails = 5,
  #                                       traceEachFile = FALSE,
  #                                       traceThisRoutine = TRUE,
  #                                       prepend = "")
  # 
  # return()

  fileBaseList1 <- dataFileBaseNames()
  fileBaseList2 <- evaluateFileListResultsAcrossProjects(fileBaseList1,
                                                         sourceDir,
                                                         referenceDir,
                                                         doFilter = TRUE,
                                                         maxNFails = 5,
                                                         traceEachFile = FALSE,
                                                         traceThisRoutine = TRUE,
                                                         prepend = "")
  
  if (length(fileBaseList2) > 0) {
    fileBaseList3 <- evaluateFileListResultsAcrossProjects(fileBaseList2,
                                                           sourceDir,
                                                           referenceDir,
                                                           doFilter = TRUE,
                                                           maxNFails = 10,
                                                           traceEachFile = TRUE,
                                                           traceThisRoutine = TRUE,
                                                           prepend = "")
  }
  return()
}

library(tidyverse)
library(lubridate)

testWorkDir <- function() {
  return("./DATA/STATIC2/US_State_Total_Test_Results_Work/")
}

dataDir <- function() {
  return("./DATA/")
}

diffTibblePath <- function() {
  return(paste(testWorkDir(), "DiffTibble.csv", sep = ""))
}

testDataPath <- function() {
  return(paste(testWorkDir(), "TestData.csv", sep = ""))
}

expectedDataPath <- function() {
  return(paste(testWorkDir(), "ExpectedTestResult.csv", sep = ""))
}

testDataPath2 <- function() {
  return(paste(testWorkDir(), "US_State_TTR_Bad.csv", sep = ""))
}

expectedDataPath2 <- function() {
  return(paste(testWorkDir(), "US_State_TTR_Desired.csv", sep = ""))
}

identityVectorXform <- function(aVector) {
  return(aVector)
}

smoothVectorZeroSeq <- function(aVector, subtrahendVector, minuendVector, dateDataVectorUNUSED) {
  theEnd = length(aVector)
  i <- 1
  newVector <- rep(1, theEnd)
  while (i <= theEnd) {
    j <- i + 1
    if ((!is.na(aVector[i])) && (aVector[i] > 0)) {
      newVector[i] <- aVector[i]
      i <- i + 1
    } else {
      if (j > theEnd) {
        newVector[i] <- 1
      } else {
        while ((j <= theEnd) && ((is.na(aVector[j])) || (aVector[j] <= 0))) {
          j <- j + 1
        }
        # here, either is.na(aVector[j]) or aVector[j] > 0 or j > theEnd
        if (j > theEnd) {
          # Increment by 1 more in each position
          for (k in i:theEnd) {
            newVector[k] <- 1 + k - i
          }
        } else {
          if (!is.na(aVector[j])) {
            # We found a non-zero value. Divvy it up into
            #   (j - i) parts;
            share <- aVector[j] / (1 + j - i)
            usedUp <- 0
            for (k in i:(j - 1)) {
              newVector[k] <- round(share * (1 + k - i))
              usedUp <- usedUp + share
            }
            newVector[j] <- aVector[j]
          } else {
            # is.na(aVector[j])
          }            
        }
      }
      i <- j + 1
    }
  }
  return(newVector)
}

processZeroDiffs <- function(dateData) {
  returnMe <- dateData
  nCols <- dim(dateData)[2]
  # minuendTibble <- dateData[,2:nCols]
  # subtrahendTibble <- dateData[,1:(nCols - 1)]
  
  for (i in 1:dim(dateData)[1]) {
    # testRow <- minuendTibble[i,] - subtrahendTibble[i,]
    replacementRow <- smoothVectorZeroSeq(dateData[i, 2:nCols] - dateData[i, 1:(nCols - 1)],
                                          dateData[i, 1:(nCols - 1)],
                                          dateData[i, 2:nCols],
                                          dateData[i,])
    returnMe[i,2:nCols] <- dateData[i,1:(nCols - 1)] + replacementRow
  }

  return(returnMe)
}

getStaticTTRFile <- function() {
  inputData <- read_csv(paste(testWorkDir(), "US_State_Total_Test_Results_EST.csv", sep = ""),
                        show_col_types = FALSE)
  
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

  write_csv(outputData, "./DATA/STATIC/US_State_Total_Test_Results.csv")

  return(outputData)
}

processTibbleToEliminateZeroIncrements <- function(aTibble) {
  # Separate date data, character data
  dateData <- select(aTibble, matches("^[0-9]+/"))
  characterData <- select(aTibble, -matches("^[0-9]+/"))

  # Process date data to eliminate zero increments
  newDateData <- processZeroDiffs(dateData)
  
  # Desired result has character data, first date data, updated newer data
  mungedData <- bind_cols(characterData, newDateData)
  
  return(mungedData)
}

processRealTTR <- function() {
  originalPath <- paste(dataDir(), "US_State_Total_Test_Results.csv", sep = "")
  STTR <- read_csv(originalPath, show_col_types = FALSE)
  
  dateLimitedSTTR <- discardDataOutsideDateRangeFromATibble(STTR, 
                                                            mdy("08/12/2021"),
                                                            mdy("01/01/2022"),
                                                            traceThisRoutine = FALSE)
  estSTTR <- processTibbleToEliminateZeroIncrements(dateLimitedSTTR)

  newPath <- paste(dataDir(), "US_State_Total_Test_Results_EST2.csv", sep = "")
  write_csv(estSTTR, newPath)
}

checkTTRModForNA <- function() {
  newPath1 <- paste(dataDir(), "US_State_Total_Test_Results_EST.csv", sep = "")
  newPath2 <- paste(dataDir(), "US_State_Total_Test_Results_EST2.csv", sep = "")
  
  tibble1 <- read_csv(newPath1, show_col_types = FALSE) %>%
    select(-Combined_Key)
  tibble2 <- read_csv(newPath2, show_col_types = FALSE) %>%
    select(-Combined_Key)
  
  diff <- tibble1 - tibble2
  
}

rowComparison <- function(testVector, expectedVector, quiet = TRUE) {
  nFailures <- 0
  vectorLength <- length(testVector)
  if (vectorLength != length(expectedVector)) {
    cat(file = stderr(), "Lengths different!", vectorLength,
        "vs.", length(expectedVector), "\n")
    nFailures <- 1
  } else {
    for (i in 1:vectorLength) {
      if (as.integer(testVector[i]) != as.integer(expectedVector[i])) {
        if (!quiet) {
          cat(file = stderr(), "mismatch at index", i,
              "found", as.integer(testVector[i]),
              "expected", as.integer(expectedVector[i]), "\n")
        }
        nFailures <- nFailures + 1
      }
    }
  }
  return(nFailures)
}

tibbleComparison <- function(processedTibble, expectedTibble, quiet = FALSE) {
  nFailures <- 0
  nRowsInProcessedTibble <- dim(processedTibble)[1]
  if (nRowsInProcessedTibble != dim(expectedTibble)[1]) {
    if (!quiet) {
      cat(file = stderr(), "Numbers of rows different!", nRowsInProcessedTibble,
          "vs.", dim(expectedTibble)[1], "\n")
    }
    nFailures <- 1
  } else {
    for (i in 1:nRowsInProcessedTibble) {
      testRow <- as.vector(processedTibble[i,])
      expectedRow <- as.vector(expectedTibble[i,])
      nRowFailures <- rowComparison(testRow, expectedRow, quiet = TRUE)
      if (nRowFailures > 0) {
        nFailures <- nFailures + nRowFailures
        if (!quiet) {
          cat(file = stderr(), nRowFailures, "mismatches in row", i, "\n")
        }
      }
    }
  }
  if ((nFailures > 0) && (!quiet)) {
    cat(file = stderr(), nFailures, "mismatches overall\n")
  }
  return(nFailures)
}

testRowComparison <- function(modifiedData, expectedData, testRows, expectNFails, quiet = FALSE) {
  nFailures <- 0
  for (i in 1:length(testRows)) {
    expectNFailuresInRow <- expectNFails[i]
    if (!quiet) {
      if (expectNFails[i] > 0) {
        cat(file = stderr(), "Row", i, "expect", expectNFails[i], "mismatch(es)\n")
      } else {
        cat(file = stderr(), "Row", i, "expect no output\n")
      }
    }
    nRowFailures <- rowComparison(as.vector(modifiedData[i,]), as.vector(expectedData[i,]),
                               quiet = FALSE)
    if (!quiet) {
      if (nRowFailures == expectNFails[i]) {
        cat(file = stderr(), "PASS\n\n")
      } else {
        cat(file = stderr(), "FAIL, expected", expectNFails[i],
            "failures, saw", nFailures, "\n\n")
      }
    }
    nFailures <- nFailures + nRowFailures
  }
  return(nFailures)
}

testTibbleComparison <- function(modifiedData, expectedData, expectNFails, quiet = FALSE) {
  nFailures <- tibbleComparison(modifiedData, expectedData, quiet = quiet)
  if (!quiet) {
    if (nFailures == expectNFails) {
      cat(file = stderr(), "PASS\n\n")
    } else {
      cat(file = stderr(), "FAIL, expected", expectNFails,
          "failures, saw", nFailures, "\n\n")
    }
  }
  return(nFailures)
}

callTests <- function() {
  expectedData <- read_csv(expectedDataPath2(), show_col_types = FALSE)
  dataToModify <- read_csv(testDataPath2(), show_col_types = FALSE)
  testRows <- c(1, 2, 3, 4)
  expectNFails = c(0, 0, 0, 0)

  cat(file = stderr(), "Test after processTibbleToEliminateZeroIncrements\n")
  correctedData <- processTibbleToEliminateZeroIncrements(dataToModify)
  nRowFailures <- testRowComparison(select(correctedData, -Combined_Key),
                                    select(expectedData, -Combined_Key),
                                    testRows, expectNFails, quiet = FALSE)
  if (nRowFailures != sum(expectNFails)) {
    cat(file = stderr(), "testRowComparison returned", nRowFailures, "failures\n")
  } else {
    cat(file = stderr(), "testRowComparison PASSES\n")
  }
  nOverallFailures <- testTibbleComparison(select(correctedData, -Combined_Key),
                                           select(expectedData, -Combined_Key),
                                           sum(expectNFails), quiet = FALSE)

  return(list(OLD = dataToModify, EXP = expectedData, NEW = correctedData))
}


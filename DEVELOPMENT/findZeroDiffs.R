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

testDataPathNA2 <- function() {
  return(paste(testWorkDir(), "US_State_TTR_BadNA2.csv", sep = ""))
}

expectedDataPathNA2 <- function() {
  return(paste(testWorkDir(), "US_State_TTR_DesiredNA2.csv", sep = ""))
}

prevBestDPathNA2 <- function() {
  return(paste(testWorkDir(), "US_State_TTR_PrevBestNA2.csv", sep = ""))
}

identityVectorXform <- function(aVector) {
  return(aVector)
}

smoothVectorZeroSeq <- function(dataVector) {
  shareAround <- function(newVector, dataVector, lastGoodIx, nextGoodIx) {
    share <- (as.double(dataVector[nextGoodIx]) - as.double(newVector[lastGoodIx])) /
      (nextGoodIx - lastGoodIx)
    for (k in (lastGoodIx + 1):(nextGoodIx - 1)) {
      newVector[k] <- newVector[lastGoodIx] + round(share * (k - lastGoodIx))
    }
    return(newVector)
  }
  
  nCols <- dim(dataVector)[2]
  newVector <- dataVector
  
  if (is.na(dataVector[1])) {
    lastGoodIx <- 0
  } else {
    lastGoodIx <- 1
  }
  
  i <- 2
  
  while (i <= nCols) {
    # Always loop through good values
    if ((!is.na(dataVector[i])) && (!is.na(newVector[i - 1])) &&
        (dataVector[i] > newVector[i - 1])) {
      if (lastGoodIx < (i - 1)) {
        if (lastGoodIx > 0) {
          # We just came off a sequence of equal values; must interpolate.
          newVector <- shareAround(newVector, dataVector, lastGoodIx, i)
          lastGoodIx <- i
        } else {
          
        }
      }
      lastGoodIx <- i
    } else {
      if (is.na(dataVector[i])) {
        # We can't use this datum.
        # If it's the last, fill 'em all out. If not, skip it.
        if (i == nCols) {
          if (lastGoodIx == 0) {
            newVector[1] <- 100
            lastGoodIx <- 1
          }
          for (k in (lastGoodIx + 1):nCols) {
            increment <- 1 # OUCH do better
            newVector[k] <- newVector[k - 1] + increment
          }
          lastGoodIx <- nCols
        }
      } else {
        # This is a number;
        # either is.na(newVector[i - 1]) or dataVector[i] <= newVector[i - 1]
        if (is.na(newVector[i - 1])) {
          # If the previous entry was NA, we can interpolate up
          #  to this one (or extrapolate if there was nothing before)
          if (lastGoodIx == 0) {
            decrement <- 1 # OUCH do better
            for (k in (i - 1):1) {
              newVector[k] <- newVector[k + 1] - decrement
            }
          } else {
            newVector <- shareAround(newVector, dataVector, lastGoodIx, i)
          }
          lastGoodIx <- i
        }
      }
    }
    i <- i + 1
  }
  
  # If we haven't yet, fill in the blanks
  if (lastGoodIx < nCols) {
    if (lastGoodIx == 0) {
      newVector[1] = 100 # OUCH do better
      lastGoodIx <- 1
    }
    for (k in (lastGoodIx + 1):nCols) {
      increment <- 1 # OUCH do better
      newVector[k] <- newVector[k - 1] + increment
    }
  }

  return(newVector)
}

processZeroDiffs <- function(dateData, quiet = TRUE) {
  returnMe <- dateData
  dims <- dim(dateData)
  nCols <- dims[2]
  nRows <- dims[1]
  
  if (!quiet) {
    cat(file = stderr(), "Entered processZeroDiffs\n")
    for (i in 1:nRows) {
      cat(file = stderr(), ".")
    }
    cat(file = stderr(), "\n")
  }

  for (i in 1:dim(dateData)[1]) {
    replacementRow <- smoothVectorZeroSeq(dateData[i,])
    returnMe[i,] <- replacementRow
    if (!quiet) {
      cat(file = stderr(), ".")
    }
  }

  if (!quiet) {
    cat(file = stderr(), "\nFinished processZeroDiffs\n\n")
  }
  
  return(returnMe)
}

getStaticTTRFile <- function() {
  inputData <- read_csv(paste(dataDir(), "US_State_Total_Test_Results_EST3.csv", sep = ""),
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

processTibbleToEliminateZeroIncrements <- function(aTibble, quiet = FALSE) {
  # Separate date data, character data
  dateData <- select(aTibble, matches("^[0-9]+/"))
  characterData <- select(aTibble, -matches("^[0-9]+/"))

  # Process date data to eliminate zero increments
  newDateData <- processZeroDiffs(round(dateData), quiet = quiet)
  
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
  estSTTR <- processTibbleToEliminateZeroIncrements(dateLimitedSTTR, quiet = FALSE)

  newPath <- paste(dataDir(), "US_State_Total_Test_Results_EST3.csv", sep = "")
  write_csv(estSTTR, newPath)
}

checkTTRModForNA <- function() {
  newPath1 <- paste(dataDir(), "US_State_Total_Test_Results_EST.csv", sep = "")
  newPath2 <- paste(dataDir(), "US_State_Total_Test_Results_EST3.csv", sep = "")
  
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
      if (is.na(testVector[i]) || is.na(expectedVector[i])) {
        if (is.na(testVector[i])) {
          cat(file = stderr(), "NA in test vector at index", i, " ")
        }
        if (is.na(expectedVector[i])) {
          cat(file = stderr(), "NA in expected vector at index", i)
        }
        cat(file = stderr(), "\n")
        nFailures <- nFailures + 1
      } else {
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
        cat(file = stderr(), "ROW PASS\n\n")
      } else {
        cat(file = stderr(), "ROW FAIL, expected", expectNFails[i],
            "failures, saw", nRowFailures, "\n\n")
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
      cat(file = stderr(), "TIBBLE PASS\n\n")
    } else {
      cat(file = stderr(), "TIBBLE FAIL, expected", expectNFails,
          "failures, saw", nFailures, "\n\n")
    }
  }
  return(nFailures)
}

callTests <- function() {
  expectedData <- read_csv(expectedDataPathNA2(), show_col_types = FALSE)
  prevBestData <- read_csv(prevBestDPathNA2(), show_col_types = FALSE)
  dataToModify <- read_csv(testDataPathNA2(), show_col_types = FALSE)
  testRows <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  prevBestNFails <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 3)
  expectNFails = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  cat(file = stderr(), "Test after processTibbleToEliminateZeroIncrements\n")
  correctedData <- processTibbleToEliminateZeroIncrements(dataToModify, quiet = FALSE)

  compareProgress <- FALSE
  if (compareProgress) {
    cat(file = stderr(), "Comparison with 'best so far'\n")
    cat(file = stderr(), "=============================\n")
    nRowFailures <- testRowComparison(select(correctedData, -Combined_Key),
                                      select(prevBestData, -Combined_Key),
                                      testRows, prevBestNFails, quiet = FALSE)
    if (nRowFailures != sum(expectNFails)) {
      cat(file = stderr(), "testRowComparison returned", nRowFailures, "failures\n")
    } else {
      cat(file = stderr(), "testRowComparison PASSES\n")
    }
    nOverallFailures <- testTibbleComparison(select(correctedData, -Combined_Key),
                                             select(prevBestData, -Combined_Key),
                                             sum(prevBestNFails), quiet = FALSE)
    cat(file = stderr(), "\n")
  }
  
  compareDesired <- TRUE
  if (compareDesired) {
    cat(file = stderr(), "Comparison with correct data\n")
    cat(file = stderr(), "============================\n")
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
  }
  
  return(correctedData)
}


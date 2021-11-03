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
  computeShare <- function(dataVector, lastGoodIx, nextGoodIx) {
    return((as.integer(dataVector[nextGoodIx]) - as.integer(dataVector[lastGoodIx])) / (nextGoodIx - lastGoodIx))
  }

  nCols <- dim(dataVector)[2]
  theEnd <- nCols
  i <- 1
  newVector <- dataVector

  while (i <= theEnd) {
    if (is.na(dataVector[i])) {
      lastGoodIx <- i - 1
      while (i <= theEnd && is.na(dataVector[i]) ) {
        i <- i + 1
        if (i > theEnd) {
          break
        }
      }
      if (lastGoodIx > 0) {
        if (i <= theEnd) {
          # DDV[lastGoodIx] and DDV[i] are both good values. Interpolate
          share <- computeShare(dataVector, lastGoodIx, i)
          for (k in (lastGoodIx + 1):(i - 1)) {
            newVector[k] <- newVector[lastGoodIx] + round(share * (k - lastGoodIx))
          }
        } else {
          # DDV[lastGoodIx] is a good value. Count up from it to the end
          for (k in (lastGoodIx + 1):theEnd) {
            newVector[k] <- newVector[k - 1] + 1
          }
        }
      } else {
        if (i <= theEnd) {
          # DDV[i] is a good value. Count down from it to the start
          for (k in 1:(i - 1)) {
            newVector[k] <- dataVector[i] - (i - k)
          }
        } else {
          # No good values. All will be phony (or estimates)
          estimate <- 100 # for testing; OUCH do better!
          for (k in (lastGoodIx + 1):theEnd) {
            newVector[k] <- estimate
            estimate <- estimate + 1
          }
        }
      }
    } else {
      while ((i < theEnd) && !(is.na(dataVector[i + 1])) &&
             (dataVector[i] != dataVector[i + 1])) {
        i <- i + 1
      }
      lastBeforeNonIncrement = i
      while ((i < theEnd) && (!is.na(dataVector[i + 1])) &&
             (dataVector[i] == dataVector[i + 1])) {
        i <- i + 1
      }
      if ((i < theEnd) && (!is.na(dataVector[i + 1]))) {
        i <- i + 1
      }
      if (i > lastBeforeNonIncrement) {
        if (i < theEnd) {
          share <- computeShare(dataVector, lastBeforeNonIncrement, i)
          for (k in (lastBeforeNonIncrement + 1):(i - 1)) {
            newVector[k] <- newVector[lastBeforeNonIncrement] + round(share * (k - lastBeforeNonIncrement))
          }
          
          cat(file = stderr(), "Interpolate from", lastBeforeNonIncrement + 1, "to", i, "\n")
          cat(file = stderr(),
              "Endpoints are", as.integer(newVector[lastBeforeNonIncrement]),
              "and", as.integer(newVector[i]), "\n")
          cat(file = stderr(), "share is", share, "\n")
          if ((i - lastBeforeNonIncrement) > 1) {
            cat(file = stderr(), "There are", i - lastBeforeNonIncrement, "slots to interpolate into\n")
          } else {
            cat(file = stderr(), "There is", i - lastBeforeNonIncrement, "slot to interpolate into\n")
          }
        } else {
          if ((i - lastBeforeNonIncrement) > 1) {
            cat(file = stderr(), "There are", i - lastBeforeNonIncrement, "slots to extrapolate into\n")
          } else {
            cat(file = stderr(), "There is", i - lastBeforeNonIncrement, "slot to extrapolate into\n")
          }
        }
      }
      i <- i + 1
    }
  }

  return(newVector)
}

smoothVectorZeroSeq2 <- function(dataVector) {
  computeShare <- function(dataVector, lastGoodIx, nextGoodIx) {
    return((as.double(dataVector[nextGoodIx]) - as.double(dataVector[lastGoodIx])) / (nextGoodIx - lastGoodIx))
  }
  
  nCols <- dim(dataVector)[2]
  theEnd <- nCols
  i <- 1
  newVector <- dataVector
  
  while (i <= theEnd) {
    if (is.na(dataVector[i]) ||
        ((i < theEnd) && !is.na(dataVector[i + 1]) &&
         (dataVector[i] == dataVector[i + 1]))) {
      if (is.na(dataVector[i])) {
        lastGoodIx <- i - 1
      } else {
        # This assumes we went through a string of NAs previously
        lastGoodIx <- i - 1
      }
      while (i <= theEnd && (is.na(dataVector[i]) ||
                             ((i < theEnd) && 
                              (is.na(dataVector[i + 1]) ||
                               ((i > 1) &&
                                !is.na(dataVector[i - 1]) &&
                                !is.na(dataVector[i]) &&
                                (dataVector[i - 1] == dataVector[i])))))) {
        i <- i + 1
      }
      # What's the next good index?
      # If we are past the end of a string of equal values, it is i + 1.
      # If we ran off the end of the vector because NAs or equal values
      #   continue to the end, there is none
      if (lastGoodIx > 0) {
        if ((i > 1) && (i <= theEnd)) {
          # DDV[lastGoodIx] and DDV[i] are both good values. Interpolate
          share <- computeShare(dataVector, lastGoodIx, i)
          for (k in (lastGoodIx + 1):(i - 1)) {
            newVector[k] <- newVector[lastGoodIx] + round(share * (k - lastGoodIx))
          }
        } else {
          # DDV[lastGoodIx] is a good value. Count up from it to the end # OUCH by an estimated increment
          for (k in (lastGoodIx + 1):theEnd) {
            newVector[k] <- newVector[k - 1] + 1
          }
        }
      } else {
        if ((i > 1) && (i <= theEnd)) {
          # DDV[i] is a good value. Count down from it to the start # OUCH by an estimated increment
          for (k in 1:(i - 1)) {
            newVector[k] <- dataVector[i] - (i - k)
          }
        } else {
          # No good values. All will be phony (or estimates)
          estimate <- 100 # for testing; OUCH do better!
          for (k in (lastGoodIx + 1):theEnd) {
            newVector[k] <- estimate
            estimate <- estimate + 1 # OUCH plus estimated increment
          }
        }
      }
    }
    i <- i + 1
  }
  #     while ((i < theEnd) && !(is.na(dataVector[i + 1])) &&
  #            (dataVector[i] != dataVector[i + 1])) {
  #       i <- i + 1
  #     }
  #     lastBeforeNonIncrement = i
  #     while ((i < theEnd) && (!is.na(dataVector[i + 1])) &&
  #            (dataVector[i] == dataVector[i + 1])) {
  #       i <- i + 1
  #     }
  #     if ((i < theEnd) && (!is.na(dataVector[i + 1]))) {
  #       i <- i + 1
  #     }
  #     if (i > lastBeforeNonIncrement) {
  #       if (i < theEnd) {
  #         share <- computeShare(dataVector, lastBeforeNonIncrement, i)
  #         for (k in (lastBeforeNonIncrement + 1):(i - 1)) {
  #           newVector[k] <- newVector[lastBeforeNonIncrement] + round(share * (k - lastBeforeNonIncrement))
  #         }
  #         
  #         cat(file = stderr(), "Interpolate from", lastBeforeNonIncrement + 1, "to", i, "\n")
  #         cat(file = stderr(),
  #             "Endpoints are", as.integer(newVector[lastBeforeNonIncrement]),
  #             "and", as.integer(newVector[i]), "\n")
  #         cat(file = stderr(), "share is", share, "\n")
  #         if ((i - lastBeforeNonIncrement) > 1) {
  #           cat(file = stderr(), "There are", i - lastBeforeNonIncrement, "slots to interpolate into\n")
  #         } else {
  #           cat(file = stderr(), "There is", i - lastBeforeNonIncrement, "slot to interpolate into\n")
  #         }
  #       } else {
  #         if ((i - lastBeforeNonIncrement) > 1) {
  #           cat(file = stderr(), "There are", i - lastBeforeNonIncrement, "slots to extrapolate into\n")
  #         } else {
  #           cat(file = stderr(), "There is", i - lastBeforeNonIncrement, "slot to extrapolate into\n")
  #         }
  #       }
  #     }
  #     i <- i + 1
  #   }
  # }
  
  return(newVector)
}

processZeroDiffs <- function(dateData) {
  returnMe <- dateData
  nCols <- dim(dateData)[2]

  for (i in 1:dim(dateData)[1]) {
    replacementRow <- smoothVectorZeroSeq2(dateData[i,])
    returnMe[i,] <- replacementRow
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
  correctedData <- processTibbleToEliminateZeroIncrements(dataToModify)

  compareProgress <- TRUE
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


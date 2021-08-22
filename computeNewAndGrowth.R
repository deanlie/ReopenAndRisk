library(tidyverse)
library(lubridate)

findColumnRangeForDate <- function(aTibble, aDate, nDays, nFirstCols=4, tibbleName="") {
  traceThisRoutine = FALSE
  # Assumes that columns 5:last can be parsed as "m/d/y"
  nCols <- dim(aTibble)[2]
  theNames <- names(aTibble)
  if (traceThisRoutine) {
    print(paste("findColumnRangeForDate: ",
                if_else(tibbleName == "",
                        "",
                        paste("tibbleName = ", tibbleName, " ", sep="")),
                "nFirstCols = ", nFirstCols,
                " theNames[nFirstCols + 1] = ", theNames[nFirstCols + 1],
                sep = ""))
  }
  earliestInTibble <- mdy(theNames[nFirstCols + 1])
  latestInTibble <- mdy(theNames[nCols])
  
  startColumn <- NA
  endColumn <- NA
  if (aDate > latestInTibble) {
    # Give 'em the last nDays that we can;
    # it covers their requested start date that's available.
    startColumn <- max(nFirstCols + 1, nCols - (nDays - 1))
    endColumn <- nCols
  } else if ((aDate - (nDays - 1)) < earliestInTibble) {
    # Give 'em the first nDays that we can; that covers
    # their requested end date if that's available.
    startColumn <- nFirstCols + 1
    endColumn <- min(nCols, startColumn + (nDays - 1))
  } else {
    endColumn <- nCols - as.integer(latestInTibble - aDate)
    startColumn <- endColumn - (nDays - 1)
  }
  list(startColumn = startColumn, endColumn = endColumn)
}

zerolessRowIndices <- function(aTibble, aColumnRange) {
  nonzero_data <- rep(TRUE, times = dim(aTibble)[1])
  for (i in aColumnRange$startColumn:aColumnRange$endColumn) {
    nonzero_data <- nonzero_data & (as.data.frame(aTibble)[,i] > 0.5)
  }
  nonzero_data
}

computeNewOnDayAndGrowthRate <- function(aTibble, aDate,
                                         nDays = 10, nFirst = 4,
                                         getGrowthRate = TRUE, nonzeroOnly = FALSE,
                                         tibbleName = "from computeNewOnDayAndGrowthRate") {
  # Get a range of nDays + 1 so you can compute nDays growth
  theRange <- findColumnRangeForDate(aTibble, aDate, nDays + 1, nFirstCols = nFirst,
                                     tibbleName = tibbleName)

  combinedKeyAndNumbers <- select(aTibble,
                                  Combined_Key,
                                  {theRange$startColumn}:{theRange$endColumn})

  if (nonzeroOnly) {  
    # Find potential zero divisors within column range
    nonzero_data <- zerolessRowIndices(aTibble, theRange)
    
    # Limit data to nonzero rows within range of dates of interest
    # Maybe fewer leading columns! Depends where Combined_Key is.  
    theData <- as.data.frame(combinedKeyAndNumbers)[nonzero_data,]
  } else {
    theData <- as.data.frame(combinedKeyAndNumbers)
  }
  
  # Thinking: I can't plot boxplots if there are any NAs, Infs, NaNs
  # But I don't want to discard whole rows just because of one zero
  # Keep the real values (including NA, Inf, -Inf, NaN) as long as
  # possible, filter them out just before plotting (clamp the ratios
  # to something really high like [-100%, +50%] growth/day)

  # There may not be enough columns after nFirst (as received by this routine)
  #  to get nDays + 1 of data. Therefore, we have to use theRange to get
  #  NewerData and OlderData.
  NewerData <- theData[, 3:(2 + theRange$endColumn - theRange$startColumn)]
  OlderData <- theData[, 2:(1 + theRange$endColumn - theRange$startColumn)]
  
  Diff       <- NewerData - OlderData
  
  JustKey    <- select(as_tibble(theData), Combined_Key)

  if (getGrowthRate) {  
    GrowthRate <- 100.0 * NewerData/OlderData - 100.0
    GrowthRate <- bind_cols(JustKey, as_tibble(GrowthRate))
  } else {
    GrowthRate <- NA
  }
  NewOnDay   <- bind_cols(JustKey, as_tibble(Diff))
  NewerData  <- bind_cols(JustKey, as_tibble(NewerData))
  
  list(growth = GrowthRate, new = NewOnDay, d2 = NewerData)
}

# Make an n-day moving average of a time series
movingAverageData <- function(aTibble, aDate, mAvgs, nDayAvg, nFirstCols=4,
                              tibbleName="from movingAverageData",
                              traceThisRoutine = FALSE) {
  theRange  <- findColumnRangeForDate(aTibble, aDate, mAvgs + nDayAvg, nFirstCols=nFirstCols,
                                      tibbleName=tibbleName)
  if (traceThisRoutine) {
    print(paste("theRange:", theRange$startColumn, theRange$endColumn))
  }
  firstForAvg <- max((nFirstCols + 1), theRange$endColumn - mAvgs + 1)
  lastForAvg  <- theRange$endColumn
  Avgs <- select(aTibble, all_of(firstForAvg):all_of(lastForAvg))
  if (traceThisRoutine) {
    print("Data for moving average computation = ")
    print(Avgs)
  }
  last_i <- max(0, min((nDayAvg - 1), ((firstForAvg - nFirstCols) - 1)))
  if (traceThisRoutine) {
    print(paste("last_i =", last_i))
  }
  if (last_i > 0) {
    for (i in 1:last_i) {
      Avgs <- Avgs + select(aTibble,
                            all_of((firstForAvg - i)):all_of((lastForAvg - i)))
    }
    Avgs <- Avgs / (last_i + 1) # if last_i < nDayAvg, that's the best we can do
  }
  if (traceThisRoutine) {
    print("Computed moving averages = ")
    print(Avgs)
  }
  
  theFirstCols  <- select(aTibble, 1:all_of(nFirstCols))
  Averages      <- bind_cols(theFirstCols, as_tibble(Avgs))
  return(Averages)
}

# Make an n-day moving average of growth of a time series

# Given a tibble with time series, 
# make a new one with the same first <k> columns, 
# and m columns each of which has n-day moving
# average of the daily changes of the input tibble,
# computed as 1/n th of the difference between day j and day (j + n)
movingAverageGrowth <- function(aTibble, aDate, mAvgs, nDayAvg, nFirstCols=4,
                                tibbleName = "from movingAverageGrowth") {
  dims = dim(aTibble)
  theRange  <- findColumnRangeForDate(aTibble, aDate, mAvgs + nDayAvg, nFirstCols=nFirstCols,
                                      tibbleName=tibbleName)
  theData   <- as.data.frame(aTibble)[, c(1:nFirstCols, theRange$startColumn:theRange$endColumn)]
  NewerData <- theData[,(nFirstCols + 1 + nDayAvg):(nFirstCols + nDayAvg + mAvgs)]
  OlderData <- theData[,(nFirstCols + 1):(nFirstCols + mAvgs)]
  Avgs      <- (NewerData - OlderData) / nDayAvg
  
  theFirstCols  <- select(as_tibble(theData), 1:all_of(nFirstCols))
  Averages      <- bind_cols(theFirstCols, as_tibble(Avgs))
}

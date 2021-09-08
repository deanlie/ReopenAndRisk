library(tidyverse)
library(lubridate)

source("./columnUtilities.R")

findColumnRangeForDate <- function(aTibble, aDate, nDays, tibbleName = "TIBBLE NAME??",
                                   traceThisRoutine = FALSE, prepend = "CALLER??") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered findColumnRangeForDate\n")
  }
  
  theNames <- names(aTibble)
  nCols <- length(theNames)
  nBeforeDates <- nColumnsBeforeDates(aTibble)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, tibbleName,
        "nBeforeDates = ", nBeforeDates,
        " theNames[nBeforeDates + 1] = ", theNames[nBeforeDates + 1],
        "\n")
  }
  earliestInTibble <- mdy(theNames[nBeforeDates + 1])
  latestInTibble <- mdy(theNames[nCols])
  
  startColumn <- NA
  endColumn <- NA

  if (aDate > latestInTibble) {
    # Give 'em the last nDays that we can;
    # it covers their requested start date that's available.
    startColumn <- max(nBeforeDates + 1, nCols - (nDays - 1))
    endColumn <- nCols
  } else if ((aDate - (nDays - 1)) < earliestInTibble) {
    # Give 'em the first nDays that we can; that covers
    # their requested end date if that's available.
    startColumn <- nBeforeDates + 1
    endColumn <- min(nCols, startColumn + (nDays - 1))
  } else {
    endColumn <- nCols - as.integer(latestInTibble - aDate)
    startColumn <- endColumn - (nDays - 1)
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving findColumnRangeForDate\n")
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
                                         nDays = 10,
                                         getGrowthRate = TRUE, nonzeroOnly = FALSE,
                                         tibbleName = "from computeNewOnDayAndGrowthRate",
                                         traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered computeNewOnDayAndGrowthRate\n")
  }
  
  # Get a range of nDays + 1 so you can compute nDays growth
  theRange <- findColumnRangeForDate(aTibble, aDate, nDays + 1,
                                     tibbleName = tibbleName,
                                     traceThisRoutine = traceThisRoutine, prepend = myPrepend)

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

  # There may not be enough columns with per-date data to get
  #  nDays + 1 of data. Therefore, we have to use theRange to get
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

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving computeNewOnDayAndGrowthRate\n")
  }
  
  list(growth = GrowthRate, new = NewOnDay, d2 = NewerData)
}

# Make an n-day moving average of a time series
movingAverageData <- function(aTibble, aDate, mAvgs, nDayAvg,
                              tibbleName="TIBBLE NAME??",
                              traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered movingAverageData for", tibbleName, "\n")
  }
  nBeforeDates <- nColumnsBeforeDates(aTibble)
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "nBeforeDates computed as", nBeforeDates, "\n")
  }

  theRange  <- findColumnRangeForDate(aTibble, aDate, mAvgs + nDayAvg,
                                      tibbleName=tibbleName,
                                      traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "theRange:", theRange$startColumn, theRange$endColumn, "\n")
  }
  firstForAvg <- max((nBeforeDates + 1), theRange$endColumn - mAvgs + 1)
  lastForAvg  <- theRange$endColumn
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "firstForAvg", firstForAvg, "lastForAvg", lastForAvg, "\n")
  }
  Avgs <- select(aTibble, {firstForAvg}:{lastForAvg})
  if (traceThisRoutine) {
    nDigits = getOption("digits")
    options(digits = 1)
    cat(file = stderr(), myPrepend, "Data for moving average computation = ", as_vector(Avgs[2,]), "\n")
    options(digits = nDigits)
  }
  last_i <- max(0, min((nDayAvg - 1), ((firstForAvg - nBeforeDates) - 1)))
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "last_i =", last_i, "\n")
  }
  if (last_i > 0) {
    for (i in 1:last_i) {
      Avgs <- Avgs + select(aTibble,
                            all_of((firstForAvg - i)):all_of((lastForAvg - i)))
    }
    Avgs <- Avgs / (last_i + 1) # if last_i < nDayAvg, that's the best we can do
  }
  if (traceThisRoutine) {
    nDigits = getOption("digits")
    options(digits = 1)
    cat(file = stderr(), myPrepend, "Computed moving averages = ", as_vector(Avgs[2,]), "\n")
    options(digits = nDigits)
  }
  
  theFirstCols  <- select(aTibble, 1:all_of(nBeforeDates))
  Averages      <- bind_cols(theFirstCols, as_tibble(Avgs))
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving movingAverageData\n")
  }
  return(Averages)
}

# Make an n-day moving average of growth of a time series

# Given a tibble with time series, 
# make a new one with the same first <k> columns, 
# and m columns each of which has n-day moving
# average of the daily changes of the input tibble,
# computed as 1/n th of the difference between day j and day (j + n)
movingAverageGrowth <- function(aTibble, aDate, mAvgs, nDayAvg,
                                tibbleName = "TIBBLE NAME??",
                              traceThisRoutine = TRUE, prepend = "CALLER??") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered movingAverageGrowth\n")
  }
  nBeforeDates <- nColumnsBeforeDates(aTibble)
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "nBeforeDates computed as", nBeforeDates, "\n")
  }

  dims = dim(aTibble)
  theRange  <- findColumnRangeForDate(aTibble, aDate, mAvgs + nDayAvg,
                                      tibbleName=tibbleName,
                                      traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "theRange:", theRange$startColumn, theRange$endColumn, "\n")
  }
  theData   <- as.data.frame(aTibble)[, c(1:nBeforeDates, theRange$startColumn:theRange$endColumn)]
  NewerData <- theData[,(nBeforeDates + 1 + nDayAvg):(nBeforeDates + nDayAvg + mAvgs)]
  OlderData <- theData[,(nBeforeDates + 1):(nBeforeDates + mAvgs)]
  Avgs      <- (NewerData - OlderData) / nDayAvg
  if (traceThisRoutine) {
    nDigits = getOption("digits")
    options(digits = 1)
    cat(file = stderr(), myPrepend, "Data for moving average computation = ", as_vector(Avgs[1,]), "\n")
    options(digits = nDigits)
  }
  
  theFirstCols  <- select(as_tibble(theData), 1:all_of(nBeforeDates))
  Averages      <- bind_cols(theFirstCols, as_tibble(Avgs))
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving movingAverageGrowth\n")
  }
  return(Averages)
}

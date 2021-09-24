# Get top and bottom 3 states for the day for some vacc stat

source("dateFormatRoutines.R")
# source("mostRecentDataDate.R")

vaccDatumKeyFromChoice <- function(vaccChoice) {
  vaccDatumLookup <- c("First Doses"="Stage_One_Doses",
                       "Second Doses"="Stage_Two_Doses",
                       "Total Doses"="Doses_admin",
                       "Fully Vaccinated"="Fully_vaccinated",
                       "People Fully Vaccinated"="People_Fully_Vaccinated")
  unname(vaccDatumLookup[vaccChoice])
}

makeFullyVaccDataIfNeeded <- function(tooMuchData, vaccChoice,
                                      traceThisRoutine = FALSE,
                                      prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered makeFullyVaccDataIfNeeded\n")
  }

  if ((vaccChoice == "People Fully Vaccinated") ||
      (vaccChoice == "Fully Vaccinated")) {
    # Use "Stage_Two_Doses" as real "Datum" entry and offset dates later
    matchMe <- "Stage_Two_Doses"
  } else {
    # Get the real "Datum" entry from the "vaccChoice" string
    matchMe <- vaccDatumKeyFromChoice(vaccChoice)
  }
  theData <- tooMuchData %>%
    filter(Datum == {matchMe})
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after filtering Stage_Two_Doses, dim(theData) = (",
        paste(dim(theData)), ")\n")
  }
  
  if ((vaccChoice == "People Fully Vaccinated") ||
      (vaccChoice == "Fully Vaccinated")) {
    # We've filtered data by "Stage_Two_Doses", now discard newest 2 weeks data
    #   and offset dates
    datumDims <- dim(theData)
    lastOldCol <- datumDims[2]
    lastNewCol <- lastOldCol - 14
    firstOldCol <- 18
    if (firstOldCol < lastOldCol) {
      lastNewCol <- 4 + lastOldCol - firstOldCol
      dataAsFrame <- as.data.frame(theData)
      mungedData <- dataAsFrame[,1:3]
      mungedData[,4:lastNewCol] = theData[,firstOldCol:lastOldCol]
      names(mungedData)[4:lastNewCol] <- names(theData)[firstOldCol:lastOldCol]
      theData <- as_tibble(mungedData) %>%
        mutate(Datum = vaccChoice, .keep = "all")
    }
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving makeFullyVaccDataIfNeeded\n")
  }

  return(theData)
}

latestVaccExtremes <- function(aTibble, vaccChoice, nTop, nBot) {
  zapCommaUS <- function(aString) {
    return(str_match(aString, "(.*), US")[,2])
  }
  theData <- makeFullyVaccDataIfNeeded(aTibble, vaccChoice) %>%
    mutate(State = zapCommaUS(Combined_Key), .before = Combined_Key, .keep = "all")
  TwoCols <- theData[, c(1, dim(theData)[2])]
  names(TwoCols) <- c("State", "Last")
  arrangedTibble <- as_tibble(TwoCols) %>%
    arrange(Last)
  topStuff <- arrangedTibble %>%
    slice_head(n = nTop)
  bottomStuff <- arrangedTibble %>%
    slice_tail(n = nBot)
  list(arranged = arrangedTibble,
       top = topStuff,
       bot = bottomStuff)
}

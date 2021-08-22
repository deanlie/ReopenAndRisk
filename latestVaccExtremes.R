# Get top and bottom 3 states for the day for some vacc stat

source("dateFormatRoutines.R")
# source("mostRecentDataDate.R")

vaccDatumKeyFromChoice <- function(vaccChoice) {
  vaccDatumLookup <- c("First Doses"="Stage_One_Doses",
                       "Second Doses"="Stage_Two_Doses",
                       "Total Doses"="Doses_admin",
                       "Fully Vaccinated"="Fully_vaccinated")
  unname(vaccDatumLookup[vaccChoice])
}

makeFullyVaccDataIfNeeded <- function(tooMuchData, vaccChoice) {
  if (vaccChoice == "Fully Vaccinated") {
    # Use "State_Two_Doses" as real "Datum" entry and offset dates later
    matchMe <- "Stage_Two_Doses"
  } else {
    # Get the real "Datum" entry from the "vaccChoice" string
    matchMe <- vaccDatumKeyFromChoice(vaccChoice)
  }
  theData <- tooMuchData %>%
    filter(Datum == matchMe)
  
  if (vaccChoice == "Fully Vaccinated") {
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
        mutate(Datum = "Fully Vaccinated", .keep = "all")
    }
  }
  return(theData)
}

latestVaccExtremes <- function(aTibble, vaccChoice, nTop, nBot) {
  theData <- makeFullyVaccDataIfNeeded(aTibble, vaccChoice)
  TwoCols <- theData[, c(2, dim(theData)[2])]
  names(TwoCols) <- c("Combined_Key", "Last")
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
# 
# foo <- latestVaccExtremes(US_State_Vaccination_Pcts, "Second Doses", 5, 5)
# View(foo$top)
# View(foo$bot)
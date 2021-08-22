# Get top and bottom 3 states for the day for some vacc stat

source("dateFormatRoutines.R")
# source("mostRecentDataDate.R")

vaccDatumKeyFromChoice <- function(vaccChoice) {
  vaccDatumLookup <- c("First Doses"="Stage_One_Doses",
                       "Second Doses"="Stage_Two_Doses",
                       "Total Doses"="Doses_admin",
                       "People Fully Vaccinated"="People_Fully_Vaccinated")
  unname(vaccDatumLookup[vaccChoice])
}

makeFullyVaccDataIfNeeded <- function(tooMuchData, vaccChoice) {
    theData <- tooMuchData %>%
      filter(Datum == vaccDatumKeyFromChoice(vaccChoice))

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
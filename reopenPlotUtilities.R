source("./state_abbrev_lookup.R")

plotTitle <- function(baseTitle, forBoxplot, justUS, movingAvg,
                      justStates = TRUE, state1 = "") {
  title <- baseTitle
  if (forBoxplot) {
    if (justStates) {
      title <- paste(title, ", State Distribution", sep = "")
    } else {
      title <- paste(title, ", County distribution", sep = "")
    }
  } else {
    if (justUS) {
      title <- paste(title, ", US Overall", sep = "")
    } else {
      title <- paste(title, ", Selected", sep = "")
      if (justStates) {
        title <- paste(title, "States")
      } else {
        title <- paste(title, admin1TypeFor(state1)$UC_PL)
      }
    }
  }
  if (movingAvg) {
    title <- paste(title, ", 7 Day Moving Average", sep = "")
  }
  title
}

timeWindowXLabel <- function(timeWindow) {
  paste("Last", timeWindow, "days")
}

filterToStateChoice <- function(aTibble, aStateChoice, countyChoices = NULL) {
  selectedState <- stateLookup[aStateChoice]
  if (is.na(countyChoices) || is.null(countyChoices)) {
    result <- aTibble %>%
      filter(Province_State == selectedState)
  } else {
    result <- aTibble %>%
      filter(stri_detect_fixed(Combined_Key, {{selectedState}}))
    if ("Admin2" %in% names(result)) {
      result <- result %>%
        filter(stri_detect_fixed(Admin2, "Out of", negate = TRUE)) %>%
        filter(stri_detect_fixed(Admin2, "Unassigned", negate = TRUE))
    }
  }
  return(result)
}



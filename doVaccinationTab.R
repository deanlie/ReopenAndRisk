source("latestVaccExtremes.R")

activeVaccData <- function(forBoxplot, movingAvg, stateChoices) {
  if (forBoxplot ||
      ((!is.na(stateChoices)) &&
       (!is.null(stateChoices)) &&
       (length(stateChoices) > 0))) {
    if (movingAvg) {
      activeData <- US_State_Vaccination_Pcts_Avg
    } else {
      activeData <- US_State_Vaccination_Pcts
    }
  } else {
    if (movingAvg) {
      activeData <- US_Vaccination_Pcts_Avg
    } else {
      activeData <- US_Vaccination_Pcts
    }
  }
  activeData
}

vaccHeaderHTML <- function(movingAvg, vaccChoice,
                           traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered vaccHeaderHTML\n")
    cat(file = stderr(), myPrepend, "vaccChoice =", vaccChoice, "\n")
  }

  tooMuchData <- activeVaccData(TRUE, movingAvg, c())

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after tooMuchData <- \n")
  }

  nMin <- 3
  nMax <- 3
  extremaStates <- latestVaccExtremes(tooMuchData, vaccChoice, nMin, nMax)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after extremaStates <- \n")
  }

  theMaxPct <- format(as.double(extremaStates$bot[nMax, 2]), digits=3)
  max2Pct   <- format(as.double(extremaStates$bot[nMax - 1, 2]), digits=3)
  theMinPct <- format(as.double(extremaStates$top[1, 2]), digits=3)
  min2Pct   <- format(as.double(extremaStates$top[2, 2]), digits=3)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "theMaxPct =", theMaxPct, "\n")
    cat(file = stderr(), myPrepend, "max2Pct =", max2Pct, "\n")
    cat(file = stderr(), myPrepend, "theMinPct =", theMinPct, "\n")
    cat(file = stderr(), myPrepend, "min2Pct =", min2Pct, "\n")
  }

  theText <- paste(tags$h4(paste("Vaccinations,", vaccChoice)),
                   tags$p("Vaccination data is shown by percent of state or of US as a whole."),
                   tags$p(paste("Highest ", vaccChoice, " rate: ",
                                extremaStates$bot[nMax, 1],
                                " with ", theMaxPct,
                                " percent", sep = "")),
                   tags$p(paste("Next highest rate: ",
                                extremaStates$bot[nMax - 1, 1],
                                " with ", max2Pct,
                                " percent", sep = "")),
                   tags$p(paste("Lowest ", vaccChoice, " rate: ",
                                extremaStates$top[1, 1],
                                " with ", theMinPct,
                                " percent", sep = "")),
                   tags$p(paste("Next lowest rate: ",
                                extremaStates$top[2, 1],
                                " with ", min2Pct,
                                " percent", sep = "")),
                   tags$p("Note that 'Total Doses' will be above 100% when close to 50% of the population
                            has had a second dose!"),
                   sep="")

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving vaccHeaderHTML\n")
  }

  HTML(theText)
}

vaccRBoxHTML <- function(movingAvg, vaccChoice) {
}

vaccRTrendHTML <- function(movingAvg, vaccChoice) {
}

vaccDataHTML <- function(movingAvg, vaccChoice,
                           traceThisRoutine = FALSE, prepend = "") {
}

vaccPlotTitle <- function(vaccChoice, forBoxplot, justUS, movingAvg) {
  baseTitleLookup <- c("First Doses"="First Vaccine Doses",
                       "Second Doses"="Second Vaccine Doses",
                       "Total Doses"="Total Vaccine Doses Administered",
                       "People Fully Vaccinated"="People Fully Vaccinated")
  title <- plotTitle(unname(baseTitleLookup[vaccChoice]), forBoxplot, justUS, movingAvg)
}

vaccDataTitle <- function(vaccChoice, movingAvg) {
  
}

vaccDataSubtitle <- function(vaccChoice, movingAvg) {
  subtitleLookup <- c("First Doses"="with at least first dose",
                      "Second Doses"="with at least second dose",
                      "Total Doses"="Total Vaccine Doses Administered",
                      "People Fully Vaccinated"="fully vaccinated")
  
  if (vaccChoice == "Total Doses") {
    partialSub <- "Total doses as percent of state population"
  } else {
    partialSub <- paste("Percent of state population",
                        subtitleLookup[vaccChoice],
                        sep = " ")
  }
  if (movingAvg) {
    partialSub <- paste(partialSub, ", 7 day moving average", sep = " ")
  }
}

vaccYLabel <- function() {
  "Vaccinations, percent of population"
}

plotVaccBoxplots <- function(movingAvg, vaccChoice, stateChoices, timeWindow,
                             traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered plotVaccBoxplots\n")
    cat(file = stderr(), myPrepend, "vaccChoice =", vaccChoice, "\n")
  }

  tooMuchData <- activeVaccData(TRUE, movingAvg, stateChoices)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "dim(tooMuchData) = (", paste(dim(tooMuchData)), ")\n")
  }

  theData <- makeFullyVaccDataIfNeeded(tooMuchData, vaccChoice,
                                       traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "dim(theData) = (", paste(dim(theData)), ")\n")
  }
  
  timeWindow <- min(timeWindow, dim(theData)[2] - 4)
  
  result <- assembleDirectBoxPlot(theData, FALSE, c(""),
                                  stateChoices,
                                  vaccPlotTitle(vaccChoice,
                                                TRUE,
                                                is.null(stateChoices),
                                                movingAvg),
                                  timeWindowXLabel(timeWindow),
                                  vaccYLabel(),
                                  clampFactor = 3, timeWindow = timeWindow,
                                  traceThisRoutine = traceThisRoutine, prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving plotVaccBoxplots\n")
  }
  
  return(result)
}

plotVaccTrend <- function(movingAvg, vaccChoice, stateChoices, timeWindow,
                          traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered plotVaccTrend\n")
    cat(file = stderr(), myPrepend, "vaccChoice =", vaccChoice, "\n")
  }

  tooMuchData <- activeVaccData(FALSE, movingAvg, stateChoices)

  theData <- makeFullyVaccDataIfNeeded(tooMuchData, vaccChoice)

  timeWindow = min(timeWindow, dim(theData)[2] - 4)

  result <- assembleDirectTrendPlot(theData, FALSE,
                                    NULL,
                                    stateChoices,
                                    vaccPlotTitle(vaccChoice,
                                                  FALSE,
                                                  is.null(stateChoices),
                                                  movingAvg),
                                    timeWindowXLabel(timeWindow),
                                    vaccYLabel(),
                                    timeWindow = timeWindow,
                                    tibbleName = "from plotVaccTrend",
                                    traceThisRoutine = traceThisRoutine, prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving plotVaccTrend\n")
  }
  
  return(result)
}

presentVaccData <- function(movingAvg, vaccChoice, stateChoices, timeWindow,
                            traceThisRoutine = FALSE, prepend = "") {
  
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered presentVaccData\n")
    cat(file = stderr(), myPrepend, "vaccChoice =", vaccChoice, "\n")
  }
  
  tooMuchData <- activeVaccData(TRUE, movingAvg, stateChoices)
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "dim(tooMuchData) = (", paste(dim(tooMuchData)), ")\n")
  }
  
  theData <- makeFullyVaccDataIfNeeded(tooMuchData, vaccChoice,
                                       traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  theData <- theData %>%
    mutate(State = str_replace(Combined_Key, ", US", ""),
           .before = 1, .keep = "unused") %>%
    select(-Population) %>%
    select(-contains("Datum"))
    
  result <- makeGtPresentation(theData,
                               stateChoices,
                               character(0),
                               "Vaccinations",
                               vaccDataSubtitle(vaccChoice, movingAvg)) %>%
    styleSelectedLines(stateChoices, character(0))

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving presentVaccData\n")
  }
  
  return(result)
}
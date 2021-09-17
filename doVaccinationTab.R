source("latestVaccExtremes.R")

activeVaccData <- function(forBoxplot, justUS, movingAvg) {
  if (forBoxplot || !justUS) {
    if (movingAvg) {
      activeData <- US_State_Vaccination_Pcts_A7
    } else {
      activeData <- US_State_Vaccination_Pcts
    }
  } else {
    if (movingAvg) {
      activeData <- US_Vaccination_Pcts_A7
    } else {
      activeData <- US_Vaccination_Pcts
    }
  }
  activeData
}

filteredVaccData <- function(forBoxplot, justUS, movingAvg, vaccChoice) {
  theData <- activeVaccData(forBoxplot, justUS, movingAvg) %>%
    filter(Datum == vaccDatumKeyFromChoice(vaccChoice))
}

vaccHeaderHTML <- function(movingAvg, vaccChoice,
                           traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered vaccHeaderHTML\n")
    cat(file = stderr(), myPrepend, "vaccChoice =", vaccChoice, "\n")
  }

  theData <- filteredVaccData(TRUE, FALSE, movingAvg, vaccChoice)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "after theData <- \n")
  }
  
  nMin <- 3
  nMax <- 3
  extremaStates <- latestVaccExtremes(theData, vaccChoice, nMin, nMax)

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

vaccPlotTitle <- function(vaccChoice, forBoxplot, justUS, movingAvg) {
  baseTitleLookup <- c("First Doses"="First Vaccine Doses",
                       "Second Doses"="Second Vaccine Doses",
                       "Total Doses"="Total Vaccine Doses Administered",
                       "People Fully Vaccinated"="People Fully Vaccinated")
  title <- plotTitle(unname(baseTitleLookup[vaccChoice]), forBoxplot, justUS, movingAvg)
}

vaccYLabel <- function() {
  "Vaccinations, percent of population"
}

plotVaccBoxplots <- function(movingAvg, vaccChoice, stateChoices, timeWindow) {
  theData <- filteredVaccData(TRUE, is.null(stateChoices), movingAvg, vaccChoice)
  
  timeWindow <- min(timeWindow, dim(theData)[2] - 4)
  
  assembleDirectBoxPlot(theData, FALSE, NULL,
                        stateChoices,
                        vaccPlotTitle(vaccChoice,
                                      TRUE,
                                      is.null(stateChoices),
                                      movingAvg),
                        timeWindowXLabel(timeWindow),
                        vaccYLabel(),
                        clampFactor = 3, timeWindow = timeWindow)
}

plotVaccTrend <- function(movingAvg, vaccChoice, stateChoices, timeWindow) {
  theData <- filteredVaccData(FALSE, is.null(stateChoices), movingAvg, vaccChoice)
  
  timeWindow = min(timeWindow, dim(theData)[2] - 4)
  
  assembleDirectTrendPlot(theData, FALSE,
                          NULL,
                          stateChoices,
                          vaccPlotTitle(vaccChoice,
                                        FALSE,
                                        is.null(stateChoices),
                                        movingAvg),
                          timeWindowXLabel(timeWindow),
                          vaccYLabel(),
                          timeWindow = timeWindow,
                          tibbleName = "plotVaccTrend's 'theData'")
}

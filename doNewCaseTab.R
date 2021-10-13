source("state_abbrev_lookup.R")
source("assemblePlotObject.R")
source("reopenPlotUtilities.R")

dataForNewCasePlots <- function(forBoxplots, countyChoices, movingAvg, stateChoices) {
  if ((!forBoxplots) && is.null(stateChoices)) {
    if (movingAvg) {
      theData <- US_Confirmed_Per100K_NewAvg
    } else {
      theData <- US_Confirmed_Per100K_New
    }
  } else {
    if (is.null(countyChoices)) {
      if (movingAvg) {
        theData <- US_State_Confirmed_Per100K_NewAvg
      } else {
        theData <- US_State_Confirmed_Per100K_New
      }
    } else {
      if (movingAvg) {
        dataTibble <- US_County_Confirmed_Per100K_NewAvg
      } else {
        dataTibble <- US_County_Confirmed_Per100K_New
      }
      theData <- filterToStateChoice(dataTibble, stateChoices[1], countyChoices)
    }
  }
  theData
}

newCaseHeaderHTML <- function(movingAvg) {
  if (!movingAvg) {
    movingAvgPgph <- movingAvgWhy()
  } else {
    movingAvgPgph <- character(0)
  }
  HTML(paste(tags$h4("Trends of new cases"),
             tags$p("We are hoping to see a steady downward trend of new cases."),
             movingAvgPgph,
             tags$p(),
             sep=""))
}

newCasePlotTitle <- function(forBoxplot, justUS, movingAvg, justStates, state1) {
  title <- plotTitle("New Covid Cases", forBoxplot, justUS, movingAvg, justStates, state1)
}

newCaseYLabel <- function() {
  "New cases per 100,000 population"
}

plotNewCaseBoxplots <- function(chooseCounty,
                                movingAvg,
                                countyChoices,
                                stateChoices,
                                timeWindow,
                                traceThisRoutine = FALSE,
                                prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered plotNewCaseBoxplots\n")
  }
  if (is.null(stateChoices)) {
    theData <- dataForNewCasePlots(TRUE, NULL, movingAvg, stateChoices)
  } else {
    theData <- dataForNewCasePlots(TRUE, countyChoices, movingAvg, stateChoices)
  }
  
  if (traceThisRoutine) {
    junk <- conciseEndsOfTibbleRow(theData,
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
    cat(file = stderr(), myPrepend, "chooseCounty:", chooseCounty, "\n")
    cat(file = stderr(), myPrepend, "countyChoices:", paste(countyChoices), "\n")
  }

  result <- assembleDirectBoxPlot(theData, chooseCounty,
                                  countyChoices, stateChoices,
                                  newCasePlotTitle(TRUE, is.null(stateChoices), movingAvg,
                                                   is.null(countyChoices), stateChoices[1]),
                                  timeWindowXLabel(timeWindow),
                                  newCaseYLabel(),
                                  clampFactor = 3, timeWindow = timeWindow,
                                  traceThisRoutine = traceThisRoutine,
                                  prepend = myPrepend)
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving plotNewCaseBoxplots\n")
  }
  
  return(result)
}

plotNewCaseTrend <- function(chooseCounty,
                             movingAvg,
                             countyChoices,
                             stateChoices,
                             timeWindow,
                             traceThisRoutine = FALSE,
                             prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered plotNewCaseTrend\n")
  }
  theData <- dataForNewCasePlots(FALSE, countyChoices, movingAvg, stateChoices)
  
  if (traceThisRoutine) {
    junk <- conciseEndsOfTibbleRow(theData,
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
    cat(file = stderr(), myPrepend, "chooseCounty:", chooseCounty, "\n")
    cat(file = stderr(), myPrepend, "countyChoices:", paste(countyChoices), "\n")
  }
  
  result <- assembleDirectTrendPlot(theData, chooseCounty,
                                    countyChoices, stateChoices,
                                    newCasePlotTitle(FALSE, is.null(stateChoices), movingAvg,
                                                     is.null(countyChoices), stateChoices[1]),
                                    timeWindowXLabel(timeWindow),
                                    newCaseYLabel(),
                                    timeWindow,
                                    traceThisRoutine = traceThisRoutine,
                                    prepend = myPrepend)
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving plotNewCaseTrend \n")
  }
  
  return(result)
}

presentNewCaseData <- function(movingAvg, countyChoices,
                               stateChoices, timeWindow,
                               traceThisRoutine = FALSE,
                               prepend = "") {
  
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered presentNewCaseData\n")
  }
  if (is.null(stateChoices)) {
    theData <- dataForNewCasePlots(TRUE, NULL, movingAvg, stateChoices)
  } else {
    theData <- dataForNewCasePlots(TRUE, countyChoices, movingAvg, stateChoices)
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "theData names:",
        paste(names(theData[c(1:5)]), sep = " "), "\n")
  }

  theData <- cleanDataForPresentation(theData,
                                      stateChoices,
                                      countyChoices)
    
  result <- makeGtPresentation(theData,
                               stateChoices,
                               countyChoices,
                               "New Cases",
                               "number of new cases per 100,000 population") %>%
    styleSelectedLines(stateChoices, countyChoices)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving presentNewCasesData\n")
  }
  
  return(result)
}

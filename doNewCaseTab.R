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

newCaseHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
  if (chooseCounty && (length(countyChoices) > 0)) {
    admin1Ts <- admin1TypeFor(stateChoices[1])$LC_PL
    middle_blank_1 <- paste(admin1Ts, " in",
                            stateLookup[stateChoices[1]])
    middle_blank_2 <- admin1Ts
  } else {
    middle_blank_1 <- "states"
    middle_blank_2 <- "states"
  }
  pgphTwo <- paste("How to interpret these charts:
 The bar across the middle of the box gives the median of all",
                      middle_blank_1,
                      "on that date. Half the",
                      middle_blank_2,
                      "fall within the box,
 a quarter above it and a quarter below it. Dots in a horizontal line
 near the top of the graph are not real data, but mean there is a data point
 at that level or higher (if the graph were scaled to show everything,
 the interesting part of the graph would be all squished at the bottom)")

  theText <- paste(tags$h4("Trends of new cases"),
                   tags$p("The CDC's recommendation was that a state not begin reopening
                   after the initial lockdown until it had a downward trajectory or
                   near-zero incidence of documented cases over a 14 day period.
                   The trend of cases is still an important measure."),
                   tags$p(pgphTwo),
                   tags$p(),
                   sep="")
  HTML(theText)
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

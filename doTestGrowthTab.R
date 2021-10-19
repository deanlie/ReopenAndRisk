source("./state_abbrev_lookup.R")
source("./assemblePlotObject.R")

testGrowthHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
  theText <- paste(tags$h4("Trends in Testing"),
                   tags$p("The data used for this tab is not updated as regularly as the
                            data for cases and mortality, and is not always reliable. Uneven updates
                            can result in numbers which change the scale so much that the resulting
                            chart is unreadable. To prevent that, these graphs do not display data
                            which is far outside the range of the bulk of the data. Dots along the
                            top or bottom of the chart are not real data."),
                   tags$p("Data displayed here can vary rapidly depending on local outbreaks and
                          testing requirements"
                          ),
                   sep="")
  HTML(theText)
}

dataForTestingRateTab <- function(forBoxplots, countyChoices, movingAvg, stateChoices) {
  if ((!forBoxplots) && is.null(stateChoices)) {
    if (movingAvg) {
      theData <- US_People_Tested_Per100_NewAvg
    } else {
      theData <- US_People_Tested_Per100_New
    }
  } else {
    if (is.null(countyChoices)) {
      if (movingAvg) {
        theData <- US_State_People_Tested_Per100_NewAvg
      } else {
        theData <- US_State_People_Tested_Per100_New
      }
    } else {
      if (movingAvg) {
        dataTibble <- US_County_People_Tested_Per100_NewAvg
      } else {
        dataTibble <- US_County_People_Tested_Per100_New
      }
      theData <- filterToStateChoice(dataTibble, stateChoices[1], countyChoices)
    }
  }
  theData
}

testingRatePlotTitle <- function(forBoxplot, justUS, movingAvg, justStates, state1) {
  title <- plotTitle("Testing Rate", forBoxplot, justUS, movingAvg, justStates, state1)
}

testingRateYLabel <- function() {
  "Testing rate, percent of population"
}

plotTestingRateBoxplots <- function(chooseCounty,
                                    movingAvg,
                                    countyChoices,
                                    stateChoices,
                                    timeWindow,
                                    traceThisRoutine = FALSE,
                                    prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered plotTestingRateBoxplots\n")
  }

  theData <- dataForTestingRateTab(TRUE, countyChoices, movingAvg, stateChoices)
  
  result <- assembleDirectBoxPlot(theData, chooseCounty,
                                  countyChoices, stateChoices,
                                  testingRatePlotTitle(TRUE, is.null(stateChoices), movingAvg,
                                                   is.null(countyChoices), stateChoices[1]),
                                  timeWindowXLabel(timeWindow),
                                  testingRateYLabel(),
                                  clampFactor = 3, timeWindow = timeWindow,
                                  traceThisRoutine = traceThisRoutine,
                                  prepend = myPrepend)

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving plotTestingRateBoxplots\n")
  }
  
  return(result)
}

plotTestGrowthBoxplots <- function(chooseCounty, movingAvg, countyChoices,
                                   stateChoices, timeWindow) {
  theData <- dataForTestingRateTab(TRUE, countyChoices, movingAvg, stateChoices)

  # County data is not available, passing data to plot routine just
  #   results in loss of dots for selected states
  assembleGrowthBoxPlot(theData,
                        FALSE, # chooseCounty,
                        NULL, # countyChoices,
                        stateChoices,
                        testingRatePlotTitle(TRUE, is.null(stateChoices), movingAvg,
                                             is.null(countyChoices), stateChoices[1]),
                        timeWindowXLabel(timeWindow),
                        testingRateYLabel(),
                        clampFactor = 1, timeWindow = timeWindow)
}

plotTestGrowthTrend <- function(chooseCounty, movingAvg, countyChoices,
                                stateChoices, timeWindow) {
  theData <- dataForTestingRateTab(FALSE, countyChoices, movingAvg, stateChoices)
  assembleDirectTrendPlot(theData,
                          FALSE, # chooseCounty,
                          NULL, # countyChoices,
                          stateChoices,
                          testingRatePlotTitle(TRUE, is.null(stateChoices), movingAvg,
                                               is.null(countyChoices), stateChoices[1]),
                          timeWindowXLabel(timeWindow),
                          testingRateYLabel(),
                          timeWindow)
}

presentTestGrowthData <- function(movingAvg, countyChoices,
                                  stateChoices, timeWindow,
                                  traceThisRoutine = FALSE,
                                  prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered presentTestGrowthData\n")
  }
  
  result <- makeGtPresentationForTab(dataForTestingRateTab, FALSE, movingAvg,
                                     stateChoices, countyChoices,
                                     "Testing Rate",
                                     "Percent of population tested that day",
                                     theID = "testRate",
                                     nDecimals = 3,
                                     traceThisRoutine = traceThisRoutine,
                                     prepend = myPrepend) 

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered presentTestGrowthData\n")
  }

  return(result)
}

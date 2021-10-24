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
    if (movingAvg) {
      theData <- US_State_People_Tested_Per100_NewAvg
    } else {
      theData <- US_State_People_Tested_Per100_New
    }
  }
  theData
}

testingRatePlotTitle <- function(forBoxplot, movingAvg, stateChoices) {
  title <- plotTitle_B("Testing Rate", forBoxplot, FALSE, movingAvg,
                          stateChoices, NULL)
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
  
  result <- assembleDirectBoxPlot(theData, FALSE,
                                  countyChoices, stateChoices,
                                  testingRatePlotTitle(TRUE, movingAvg, stateChoices),
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

plotTestGrowthTrend <- function(chooseCounty, movingAvg, countyChoices,
                                stateChoices, timeWindow) {
  theData <- dataForTestingRateTab(FALSE, countyChoices, movingAvg, stateChoices)
  assembleDirectTrendPlot(theData,
                          FALSE, # chooseCounty,
                          NULL, # countyChoices,
                          stateChoices,
                          testingRatePlotTitle(FALSE, movingAvg, stateChoices),
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

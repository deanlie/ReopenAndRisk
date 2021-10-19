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

dataForTestingRateTab <- function(countyChoices, movingAvg, stateChoices) {
  if (is.null(stateChoices)) {
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

plotTestGrowthBoxplots <- function(chooseCounty, movingAvg, countyChoices,
                                   stateChoices, timeWindow) {
  if (is.null(stateChoices)) {
    theData <- dataForTestingRateTab(NULL, movingAvg, "AK")
  } else {
    theData <- dataForTestingRateTab(countyChoices, movingAvg, stateChoices)
  }
  
  if (movingAvg) {
    title <- "COVID Testing Growth Distribution, 7 day moving average"
  } else {
    title <- "COVID Testing Growth Distribution"
  }
  # County data is not available, passing data to plot routine just
  #   results in loss of dots for selected states
  assembleGrowthBoxPlot(theData,
                        FALSE, # chooseCounty,
                        NULL, # countyChoices,
                        stateChoices,
                        title,
                        paste("Last", timeWindow, "days"),
                        "Daily growth rate: new day's number of tests as percent of previous number",
                        clampFactor = 1, timeWindow = timeWindow)
}

plotTestGrowthTrend <- function(chooseCounty, movingAvg, countyChoices,
                                stateChoices, timeWindow) {
  # OUCH more logic here to get US_County... sometimes
  if (is.null(stateChoices)) {
    title <- "COVID Testing Growth Trend for US as a Whole"
  } else {
    title <- "COVID Testing Growth Trends for Selected States"
  }
  theData <- dataForTestingRateTab(countyChoices, movingAvg, stateChoices)
  assembleGrowthTrendPlot(theData, FALSE, # OUCH chooseCounty,
                          NULL, # OUCH countyChoices,
                          stateChoices,
                          title,
                          paste("Last", timeWindow, "days"),
                          "Daily growth rate: new day's number of tests as percent of previous number",
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
  
  result <- makeGtPresentationForTab(dataForTestingRateTab, movingAvg,
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

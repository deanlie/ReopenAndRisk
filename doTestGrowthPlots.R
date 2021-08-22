source("./loadUSPeopleTestedData.R")
source("./state_abbrev_lookup.R")
source("./assemblePlotObject.R")

dataForTestGrowthPlots <- function(countyChoices, movingAvg, stateChoices) {
  if (is.null(stateChoices)) {
    if (movingAvg) {
      theData <- US_People_Tested_A7
    } else {
      theData <- US_People_Tested
    }
  } else {
    if (movingAvg) {
      theData <- US_State_People_Tested_A7
    } else {
      theData <- US_State_People_Tested
    }
  }
  theData
}

plotTestGrowthBoxplots <- function(chooseCounty, movingAvg, countyChoices,
                                   stateChoices, timeWindow) {
  if (is.null(stateChoices)) {
    theData <- dataForTestGrowthPlots(NULL, movingAvg, "AK")
  } else {
    theData <- dataForTestGrowthPlots(countyChoices, movingAvg, stateChoices)
  }
  
  if (movingAvg) {
    title <- "COVID Testing Growth Distribution, 7 day moving average"
  } else {
    title <- "COVID Testing Growth Distribution"
  }
  
  assembleGrowthBoxPlot(theData,
                        chooseCounty,
                        countyChoices,
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
  theData <- dataForTestGrowthPlots(countyChoices, movingAvg, stateChoices)
  assembleGrowthTrendPlot(theData, FALSE, # OUCH chooseCounty,
                          NULL, # OUCH countyChoices,
                          stateChoices,
                          title,
                          paste("Last", timeWindow, "days"),
                          "Daily growth rate: new day's number of tests as percent of previous number",
                          timeWindow)
}

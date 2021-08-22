
testResultsHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
  theText <- paste(tags$h4("Changes in Test Results"),
                   tags$p("A decreasing percentage of positive tests is good;
                            it means that tests are becoming more widely available."),
                   tags$p("Think of the extreme case: if everyone were tested,
                            and 0 percent of tests were positive, the epidemic would be over."),
                   sep="")
  HTML(theText)
}

testResultPlotTitle <- function(forBoxplot, justUS, movingAvg) {
  title <- plotTitle("TestPositivity", forBoxplot, justUS, movingAvg)
}

plotTestResultBoxplots <- function(chooseCounty, movingAvg, countyChoices,
                                   stateChoices, timeWindow) {
  # updateDataForUSTypeIfNeeded("Confirmed")
  title <- testResultPlotTitle(TRUE, length(stateChoices) == 0, movingAvg)
  
  if (movingAvg) {
    theCaseData <- US_State_Confirmed_A7
    theTestData <- US_State_People_Tested_A7 
  } else {
    theCaseData <- US_State_Confirmed
    theTestData <- US_State_People_Tested 
  }
  
  assembleRatioDeltaBoxPlot(theCaseData, theTestData, stateChoices,
                            title,
                            timeWindowXLabel(timeWindow),
                            "Test Positivity: percent of tests returning positive",
                            clampFactor = 1,
                            timeWindow = timeWindow,
                            nFirstNum = 2, nFirstDenom = 2)
}

plotTestResultTrend <- function(chooseCounty, movingAvg, countyChoices,
                                stateChoices, timeWindow) {
  # updateDataForUSTypeIfNeeded("Confirmed")
  title <- testResultPlotTitle(FALSE, length(stateChoices) == 0, movingAvg)
  
  if (is.null(stateChoices)) {
    if (movingAvg) {
      theCaseData <- US_Confirmed_A7
      theTestData <- US_People_Tested_A7
    } else {
      theCaseData <- US_Confirmed
      theTestData <- US_People_Tested
    }
  } else {
    if (movingAvg) {
      theCaseData <- US_State_Confirmed_A7
      theTestData <- US_State_People_Tested_A7
    } else {
      theCaseData <- US_State_Confirmed
      theTestData <- US_State_People_Tested
    }
  }
  
  assembleRatioDeltaTrendPlot(theCaseData, theTestData, stateChoices,
                              title,
                              timeWindowXLabel(timeWindow),
                              "Test Positivity: percent of tests returning positive",
                              timeWindow = timeWindow,
                              nFirstNum = 2, nFirstDenom = 2)
}

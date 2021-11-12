
testPositivityHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
  theText <- paste(tags$h4("Test Positivity"),
                   tags$p("A decreasing percentage of positive tests is good;
                            it means that tests are becoming more widely available."),
                   tags$p("Think of the extreme case: if everyone were tested,
                            and 0 percent of tests were positive, the epidemic would be over."),
                   sep="")
  HTML(theText)
}

dataForTestPositivityTab <- function(forBoxplots, countyChoices, movingAvg, stateChoices) {
  if ((!forBoxplots) && is.null(stateChoices)) {
    if (movingAvg) {
      theData <- US_Test_Positivity_Avg
    } else {
      theData <- US_Test_Positivity
    }
  } else {
    if (movingAvg) {
      theData <- US_State_Test_Positivity_Avg
    } else {
      theData <- US_State_Test_Positivity
    }
  }
  theData
}

testPositivityPlotTitle <- function(forBoxplot, movingAvg, stateChoices) {
  title <- plotTitle_B("TestPositivity", forBoxplot, FALSE, movingAvg,
                       stateChoices, NULL)
}

testPositivityYLabel <- function() {
  "Test Positivity: percent of tests returning positive"
}

plotTestResultBoxplots <- function(chooseCounty, movingAvg, countyChoices,
                                   stateChoices, timeWindow,
                                   traceThisRoutine = FALSE,
                                   prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered plotTestResultBoxplots\n")
  }

  theData <- dataForTestPositivityTab(TRUE, countyChoices, movingAvg, stateChoices)
  # updateDataForUSTypeIfNeeded("Confirmed")
  title <- testPositivityPlotTitle(TRUE, length(stateChoices) == 0, movingAvg)
  
  # if (movingAvg) {
  #   theCaseData <- US_State_Confirmed_Avg
  #   theTestData <- US_State_People_Tested_Avg 
  #   numTibbleName <- "US_State_Confirmed_Avg"
  #   denomTibbleName <- "US_State_People_Tested_Avg"
  # } else {
  #   theCaseData <- US_State_Confirmed
  #   theTestData <- US_State_People_Tested 
  #   numTibbleName <- "US_State_Confirmed"
  #   denomTibbleName <- "US_State_People_Tested"
  # }
  
  # result <- assembleRatioDeltaBoxPlot(theCaseData, theTestData, stateChoices,
  #                           title,
  #                           paste("Last", timeWindow, "days"),
  #                           "Test Positivity: percent of tests returning positive",
  #                           clampFactor = 1,
  #                           timeWindow = timeWindow,
  #                           numTibbleName = numTibbleName,
  #                           denomTibbleName = denomTibbleName,
  #                           traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  result <- assembleDirectBoxPlot(theData, FALSE,
                                  countyChoices, stateChoices,
                                  testPositivityPlotTitle(TRUE, movingAvg, stateChoices),
                                  timeWindowXLabel(timeWindow),
                                  testPositivityYLabel(),
                                  clampFactor = 3, timeWindow = timeWindow,
                                  traceThisRoutine = traceThisRoutine,
                                  prepend = myPrepend)
  
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving plotTestResultBoxplots\n")
  }

  return(result)
}

plotTestResultTrend <- function(chooseCounty, movingAvg, countyChoices,
                                stateChoices, timeWindow,
                                traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered plotTestResultTrend\n")
  }

  theData <- dataForTestPositivityTab(FALSE, countyChoices, movingAvg, stateChoices)
  title <- testPositivityPlotTitle(FALSE, length(stateChoices) == 0, movingAvg)

  # updateDataForUSTypeIfNeeded("Confirmed")
  # if (is.null(stateChoices)) {
  #   if (movingAvg) {
  #     theCaseData <- US_Confirmed_Avg
  #     theTestData <- US_People_Tested_Avg
  #   } else {
  #     theCaseData <- US_Confirmed
  #     theTestData <- US_People_Tested
  #   }
  # } else {
  #   if (movingAvg) {
  #     theCaseData <- US_State_Confirmed_Avg
  #     theTestData <- US_State_People_Tested_Avg
  #   } else {
  #     theCaseData <- US_State_Confirmed
  #     theTestData <- US_State_People_Tested
  #   }
  # }
  
  result <- assembleDirectTrendPlot(theData,
                                    FALSE,
                                    NULL,
                                    stateChoices,
                                    testPositivityPlotTitle(FALSE, movingAvg, stateChoices),
                                    timeWindowXLabel(timeWindow),
                                    testPositivityYLabel(),
                                    timeWindow,
                                    traceThisRoutine = traceThisRoutine,
                                    prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving plotTestResultTrend\n")
  }
  
  return(result)
}

presentTestResultsData <- function(movingAvg, countyChoices,
                                  stateChoices, timeWindow,
                                  traceThisRoutine = FALSE,
                                  prepend = "") {
  theData <- dataForTestPositivityTab(FALSE, NULL, movingAvg, stateChoices)
  result <- makeGtPresentation(theData,
                               stateChoices,
                               NULL,
                               testPositivityPlotTitle(FALSE, movingAvg, stateChoices),
                               "Percent of tests returning positive",
                               theID = "testPositivityData") %>%
    styleSelectedLines(stateChoices, NULL)
  
  return(result)
}


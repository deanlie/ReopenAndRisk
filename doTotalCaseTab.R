source("state_abbrev_lookup.R")
source("assemblePlotObject.R")
source("reopenPlotUtilities.R")

dataForTotalCasePlots <- function(forBoxplots, countyChoices, movingAvg, stateChoices) {
  if ((!forBoxplots) && is.null(stateChoices)) {
    if (movingAvg) {
      theData <- US_Confirmed_Per100KAvg
    } else {
      theData <- US_Confirmed_Per100K
    }
  } else {
    if (is.null(countyChoices) || length(countyChoices) == 0) {
      if (movingAvg) {
        theData <- US_State_Confirmed_Per100KAvg
      } else {
        theData <- US_State_Confirmed_Per100K
      }
    } else {
      if (movingAvg) {
        dataTibble <- US_County_Confirmed_Per100KAvg
      } else {
        dataTibble <- US_County_Confirmed_Per100K
      }
      theData <- filterToStateChoice(dataTibble, stateChoices[1])
    }
  }
  theData
}

totalCaseHeaderHTML <- function(movingAvg) {
  if (!movingAvg) {
    movingAvgPgph <- movingAvgWhy()
  } else {
    movingAvgPgph <- character(0)
  }
  HTML(paste(tags$h4("Trends of total cases"),
             tags$p("Let's hope total number of total cases curve continues to flatten."),
             movingAvgPgph,
             tags$p(),
             sep=""))
}

totalCasePlotTitle <- function(forBoxplot, justUS, movingAvg, justStates, state1) {
  title <- plotTitle("Total Covid Cases", forBoxplot, justUS, movingAvg, justStates, state1)
}

totalCasePlotTitle_B <- function(forBoxplot, countiesAvailable, movingAvg,
                                 stateChoices, countyChoices) {
  title <- plotTitle_B("Total Covid Cases", forBoxplot, FALSE, movingAvg,
                        stateChoices, NULL)
}
  
totalCaseYLabel <- function() {
  "Total cases per 100,000 population"
}

selectPlotData <- function(selectorRoutine, chooseCounty,
                           forBoxplot, countyChoices, movingAvg, stateChoices) {
  if (is.null(stateChoices)) {
    theData <- selectorRoutine(forBoxplot, NULL, movingAvg, stateChoices)
  } else {
    theData <- selectorRoutine(forBoxplot, countyChoices, movingAvg, stateChoices)
  }
}

plotTotalCaseBoxplots <- function(chooseCounty,
                                movingAvg,
                                countyChoices,
                                stateChoices,
                                timeWindow) {
  theData <- selectPlotData(dataForTotalCasePlots, chooseCounty,
                            TRUE, countyChoices, movingAvg, stateChoices)
  
  assembleDirectBoxPlot_B(theData, chooseCounty,
                          countyChoices, stateChoices,
                          totalCasePlotTitle_B(TRUE, FALSE, movingAvg,
                                               stateChoices, countyChoices),
                          timeWindowXLabel(timeWindow),
                          totalCaseYLabel(),
                          clampFactor = 3, timeWindow = timeWindow,
                          tibbleName = "from assembleDirectBoxPlot",
                          traceThisRoutine = FALSE, prepend = "")
}

plotTotalCaseTrend <- function(chooseCounty,
                             movingAvg,
                             countyChoices,
                             stateChoices,
                             timeWindow) {
  theData <- dataForTotalCasePlots(FALSE, countyChoices, movingAvg, stateChoices)

  assembleDirectTrendPlot(theData, chooseCounty,
                          countyChoices, stateChoices,
                          totalCasePlotTitle_B(FALSE, FALSE, movingAvg,
                                               stateChoices, countyChoices),
                          timeWindowXLabel(timeWindow),
                          totalCaseYLabel(),
                          timeWindow)
}

presentTotalCaseData <- function(movingAvg, countyChoices,
                                 stateChoices, timeWindow,
                                 traceThisRoutine = FALSE,
                                 prepend = "") {
  
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered presentTotalCaseData\n")
  }
  theData <- selectPlotData(dataForTotalCasePlots, TRUE,
                            TRUE, countyChoices, movingAvg, stateChoices)
  
  theData <- cleanDataForPresentation(theData,
                                      stateChoices,
                                      countyChoices)
  
  result <- makeGtPresentation(theData,
                               stateChoices,
                               countyChoices,
                               "Total Cases",
                               "total number of cases per 100,000 population",
                               theID = "totCases") %>%
    styleSelectedLines(stateChoices, countyChoices)
    
    if (traceThisRoutine) {
      cat(file = stderr(), prepend, "Leaving presentNewCasesData\n")
    }
    
    return(result)
  }



  
  

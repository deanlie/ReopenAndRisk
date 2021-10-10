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

totalCaseHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
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

  theText <- paste(tags$h4("Trends of total cases"),
                   tags$p("The CDC's recommendation was that a state not begin reopening
                   after the initial lockdown until it had a downward trajectory or
                   near-zero incidence of documented cases over a 14 day period.
                   The trend of cases is still an important measure."),
                   tags$p(pgphTwo),
                   tags$p(),
                   sep="")
  HTML(theText)
}

totalCasePlotTitle <- function(forBoxplot, justUS, movingAvg, justStates, state1) {
  title <- plotTitle("Total Covid Cases", forBoxplot, justUS, movingAvg, justStates, state1)
}

totalCaseYLabel <- function() {
  "Total cases per 100,000 population"
}

selectPlotData <- function(selectorRoutine, chooseCounty,
                           forBoxplot, countyChoices, movingAvg, stateChoices) {
  # if (!chooseCounty) {
  #   countyChoices <- NULL
  # }
  cat(file = stderr(), "selectPlotData: countyChoices = *",
      paste(countyChoices, sep = " "), "*\n", sep = "")

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

  assembleDirectBoxPlot(theData, chooseCounty,
                        countyChoices, stateChoices,
                        totalCasePlotTitle(TRUE, is.null(stateChoices), movingAvg,
                                           is.null(countyChoices), stateChoices[1]),
                        timeWindowXLabel(timeWindow),
                        totalCaseYLabel(),
                        clampFactor = 3, timeWindow = timeWindow)
}

plotTotalCaseTrend <- function(chooseCounty,
                             movingAvg,
                             countyChoices,
                             stateChoices,
                             timeWindow) {
  theData <- dataForTotalCasePlots(FALSE, countyChoices, movingAvg, stateChoices)

  assembleDirectTrendPlot(theData, chooseCounty,
                          countyChoices, stateChoices,
                          totalCasePlotTitle(FALSE, is.null(stateChoices), movingAvg,
                                             is.null(countyChoices), stateChoices[1]),
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
                               "total number of cases per 100,000 population") %>%
    styleSelectedLines(stateChoices, countyChoices)
    
    if (traceThisRoutine) {
      cat(file = stderr(), prepend, "Leaving presentNewCasesData\n")
    }
    
    return(result)
  }



  
  

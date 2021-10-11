source("./state_abbrev_lookup.R")
source("./assemblePlotObject.R")

library(stringr)

dataForTotalDeathsPlots <- function(forBoxplots, countyChoices, movingAvg, stateChoices) {
  if ((!forBoxplots) && is.null(stateChoices)) {
    if (movingAvg) {
      theData <- US_Deaths_Per100KAvg
    } else {
      theData <- US_Deaths_Per100K
    }
  } else {
    if (is.null(countyChoices)) {
      if (movingAvg) {
        theData <- US_State_Deaths_Per100KAvg
      } else {
        theData <- US_State_Deaths_Per100K
      }
    } else {
      detectOutOf <- function(aString) {
        str_detect(aString, "Out of", negate = TRUE)
      }
      detectUnassigned <- function(aString) {
        str_detect(aString, "Unassigned", negate = TRUE)
      }
      if (movingAvg) {
        dataTibble <- US_County_Deaths_Per100KAvg
      } else {
        dataTibble <- US_County_Deaths_Per100K
      }
      theData <- dataTibble %>%
        filter(Province_State == stateLookup[stateChoices[1]]) %>%
        filter(detectOutOf(Admin2)) %>%
        filter(detectUnassigned(Admin2))
    }
  }
  theData
}

getPopulationLimitedKeys <- function(keyList, popLimit) {
  smallPops <- filter(US_Population, Combined_Key %in% keyList, Population < popLimit)
}

getPopulationLimitedKeysText <- function(keyList, popLimit, adminTypePlural, columnKey) {
  smallPops <- getPopulationLimitedKeys(keyList, popLimit)
  smallPopTxt <- ""
  dimSP <- dim(smallPops)
  if (dimSP[1] > 0) {
    theSuchAs <- smallPops[1, columnKey]
    if (dimSP[1] > 1) {
      for (i in 2:dimSP[1]) {
        if (i == dimSP[1]) {
          if (i > 2) {
            theSuchAs <- paste(theSuchAs, ", and", sep = "")
          } else {
            theSuchAs <- paste(theSuchAs, " and", sep = "")
          }
        } else {
          theSuchAs <- paste(theSuchAs, ",", sep = "")
        }
        theSuchAs <- paste(theSuchAs, smallPops[i, columnKey])
      }
    }
    smallPopTxt <- paste("In ", adminTypePlural,
                         " with small populations, such as ", theSuchAs, 
                         ", a few deaths -- not to minimize their importance ",
                         "-- can make the statistics jump a lot ",
                         "from day to day.", sep = "")
  }
}

totalDeathsHeaderHTML <- function(movingAvg, countyChoices, stateChoices) {
  theText <- paste(tags$h4("Deaths"),
                   tags$p("By the present time, these totals change very slowly. Don't expect to see much change in this plot."),
                   sep="")
  
  if (length(countyChoices) > 0) {
    countyKeys <- c()
    for (aCounty in countyChoices) {
      TotalKeys <- makeCombinedKeys(aCounty, stateChoices[1])
      countyKeys <- c(countyKeys, newKeys$spaced, newKeys$spaceless)
    }
    admin1T <- admin1TypeFor(stateChoices[1])
    smallPopsTxt <- getPopulationLimitedKeysText(countyKeys, 100000,
                                                 admin1T$LC_PL, "CountyName")
    overlapTxt <- paste("If two or more selected ", admin1T$LC_PL,
                        " have identical graphs (e.g. zero deaths) for the selected ",
                        "time period, the last one drawn wil hide the other. ",
                        "If no graph is shown for your ",
                        admin1T$LC_S,
                        " please try again, selecting only it.")
    if ((!movingAvg) && (length(smallPopsTxt) > 0)) {
      theText <- paste(theText,
                       tags$p(smallPopsTxt),
                       movingAvgWhy(),
                       tags$p(overlapTxt),
                       sep = "")
    } else {
      theText <- paste(theText,
                       tags$p(overlapTxt),
                       sep = "")
      
    }
  } else if (length(stateChoices) > 0) {
    stateKeys <- c()
    for (aState in stateChoices) {
      newKeys <- makeCombinedKeys(NA, aState)
      stateKeys <- c(stateKeys,  newKeys$spaced, newKeys$spaceless)
    }
    smallPopsTxt <- getPopulationLimitedKeysText(stateKeys, 2000000,
                                                 "states", "Province_State")
    if ((!movingAvg) && (length(smallPopsTxt) > 0)) {
      theText <- paste(theText,
                       tags$p(smallPopsTxt),
                       movingAvgWhy(),
                       sep = "")
    }
  }
  HTML(theText)
}

totalDeathsPlotTitle <- function(forBoxplot, justUS, movingAvg, justStates, state1) {
  title <- plotTitle("Total Covid Deaths",
                     forBoxplot, justUS, movingAvg, justStates, state1)
}

totalDeathsYLabel <- function() {
  "Total deaths per 100,000 population"
}

plotTotalDeathsBoxplots <- function(chooseCounty,
                                    movingAvg,
                                    countyChoices,
                                    stateChoices,
                                    timeWindow) {
  if (is.null(stateChoices)) {
    theData <- dataForTotalDeathsPlots(TRUE, NULL, movingAvg, NULL)
  } else {
    theData <- dataForTotalDeathsPlots(TRUE, countyChoices, movingAvg, stateChoices)
  }

  assembleDirectBoxPlot(theData, chooseCounty,
                        countyChoices,
                        stateChoices,
                        totalDeathsPlotTitle(TRUE, is.null(stateChoices), movingAvg,
                                             is.null(countyChoices), stateChoices[1]),
                        timeWindowXLabel(timeWindow),
                        totalDeathsYLabel(),
                        clampFactor = 3, timeWindow = timeWindow)
}

plotTotalDeathsTrend <- function(chooseCounty,
                                 movingAvg,
                                 countyChoices,
                                 stateChoices,
                                 timeWindow) {
  theData <- dataForTotalDeathsPlots(FALSE, countyChoices, movingAvg, stateChoices)

  assembleDirectTrendPlot(theData, chooseCounty,
                          countyChoices,
                          stateChoices,
                          totalDeathsPlotTitle(FALSE, is.null(stateChoices), movingAvg,
                                               is.null(countyChoices), stateChoices[1]),
                          timeWindowXLabel(timeWindow),
                          totalDeathsYLabel(),
                          timeWindow)
}

presentTotalDeathsData <- function(movingAvg, countyChoices,
                                 stateChoices, timeWindow,
                                 traceThisRoutine = FALSE,
                                 prepend = "") {
#  return(bogusGtDisplay("presentTotalDeathsData"))
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered presentTotalDeathsData\n")
  }
  if (is.null(stateChoices)) {
    theData <- dataForTotalDeathsPlots(TRUE, NULL, movingAvg, stateChoices)
  } else {
    theData <- dataForTotalDeathsPlots(TRUE, countyChoices, movingAvg, stateChoices)
  }
  
  theData <- cleanDataForPresentation(theData,
                                      stateChoices,
                                      countyChoices)
  
  result <- makeGtPresentation(theData,
                               stateChoices,
                               countyChoices,
                               "Total Deaths",
                               "total number of deaths per 100,000 population") %>%
    styleSelectedLines(stateChoices, countyChoices)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving presentTotalDeathsData\n")
  }
  
  return(result)
}

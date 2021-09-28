source("./state_abbrev_lookup.R")
source("./assemblePlotObject.R")

library(stringr)

dataForNewDeathsPlots <- function(forBoxplots, countyChoices, movingAvg, stateChoices) {
  if ((!forBoxplots) && is.null(stateChoices)) {
    if (movingAvg) {
      theData <- US_Deaths_A7
    } else {
      theData <- US_Deaths
    }
  } else {
    if (is.null(countyChoices)) {
      if (movingAvg) {
        theData <- US_State_Deaths_A7
      } else {
        theData <- US_State_Deaths
      }
    } else {
      detectOutOf <- function(aString) {
        str_detect(aString, "Out of", negate = TRUE)
      }
      detectUnassigned <- function(aString) {
        str_detect(aString, "Unassigned", negate = TRUE)
      }
      if (movingAvg) {
        dataTibble <- US_County_Deaths_A7
      } else {
        dataTibble <- US_County_Deaths
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

newDeathsHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
  theText <- paste(tags$h4("Deaths"),
                   tags$p("The CDC does not base reopening recommendations directly on 
                             COVID-19 deaths, but we have the statistics and the graphing
                             programs, so here you go:"),
                   sep="")
  
  if (chooseCounty && (length(countyChoices) > 0)) {
    countyKeys <- c()
    for (aCounty in countyChoices) {
      newKeys <- makeCombinedKeys(aCounty, stateChoices[1])
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
    missingLineTxt <- "It's possible that lines for some areas will be missing,
 especially if you've selected some counties with low population.
 That happens because a growth rate starting from zero
 can't be computed. If you see that, please change the selection
 to have ONLY the missing areas, and I'll display the NUMBER of
 cases rather than the growth rate."
    if (length(smallPopsTxt > 0)) {
      theText <- paste(theText,
                       tags$p(smallPopsTxt),
                       tags$p(overlapTxt),
                       tags$p(missingLineTxt),
                       sep = "")
    } else {
      theText <- paste(theText,
                       tags$p(overlapTxt),
                       tags$p(missingLineTxt),
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
    if (length(smallPopsTxt > 0)) {
      theText <- paste(theText,
                       tags$p(smallPopsTxt),
                       sep = "")
    }
  }
  HTML(theText)
}

newDeathsPlotTitle <- function(forBoxplot, justUS, movingAvg, justStates, state1) {
  title <- plotTitle("New Covid Deaths",
                     forBoxplot, justUS, movingAvg, justStates, state1)
}

newDeathsYLabel <- function() {
  "New deaths per 100,000 population"
}

plotNewDeathsBoxplots <- function(chooseCounty,
                                  movingAvg,
                                  countyChoices,
                                  stateChoices,
                                  timeWindow) {
  if (is.null(stateChoices)) {
    theData <- dataForNewDeathsPlots(TRUE, NULL, movingAvg, NULL)
  } else {
    theData <- dataForNewDeathsPlots(TRUE, countyChoices, movingAvg, stateChoices)
  }

  assembleGrowthBoxPlot(theData, chooseCounty,
                        countyChoices,
                        stateChoices,
                        newDeathsPlotTitle(TRUE, is.null(stateChoices), movingAvg,
                                           is.null(countyChoices), stateChoices[1]),
                        timeWindowXLabel(timeWindow),
                        newDeathsYLabel(),
                        clampFactor = 3, timeWindow = timeWindow)
}

plotNewDeathsTrend <- function(chooseCounty,
                               movingAvg,
                               countyChoices,
                               stateChoices,
                               timeWindow) {
  theData <- dataForNewDeathsPlots(FALSE, countyChoices, movingAvg, stateChoices)

  assembleGrowthTrendPlot(theData, chooseCounty,
                          countyChoices,
                          stateChoices,
                          newDeathsPlotTitle(FALSE, is.null(stateChoices), movingAvg,
                                             is.null(countyChoices), stateChoices[1]),
                          timeWindowXLabel(timeWindow),
                          newDeathsYLabel(),
                          timeWindow)
}

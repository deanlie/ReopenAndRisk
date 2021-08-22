# source("./loadUSDeathsData.R")
source("./state_abbrev_lookup.R")
source("./assemblePlotObject.R")

library(stringr)

dataForMortalityPlots <- function(countyChoices, movingAvg, stateChoices) {
  if (is.null(stateChoices)) {
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

mortalityHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
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

plotMortalityGrowthBoxplots <- function(chooseCounty,
                                        movingAvg,
                                        countyChoices,
                                        stateChoices,
                                        timeWindow) {
  if (is.null(stateChoices)) {
    theData <- dataForMortalityPlots(NULL, movingAvg, "AK")
  } else {
    theData <- dataForMortalityPlots(countyChoices, movingAvg, stateChoices)
  }
  
  if (movingAvg) {
    title <- "COVID Mortality Growth Distribution, 7 day moving average"
  } else{
    title <- "COVID Mortality Growth Distribution"
  }

  assembleGrowthBoxPlot(theData, chooseCounty,
                        countyChoices,
                        stateChoices,
                        title,
                        paste("Last", timeWindow, "days"),
                        "Daily growth rate: new day's deaths as percent of previous total deaths",
                        clampFactor = 3, timeWindow = timeWindow)
}

plotMortalityGrowthTrend <- function(chooseCounty,
                                     movingAvg,
                                     countyChoices,
                                     stateChoices,
                                     timeWindow) {
  if (is.null(stateChoices)) {
    title <- "COVID Mortality Growth Trend for US as a whole"
  } else {
    if (chooseCounty && !(is.null(countyChoices))) {
      title <- paste("COVID Mortality Growth Trends for Selected ",
                     admin1TypeFor(stateChoices[1])$UC_PL,
                     sep = "")
    } else {
      title <- "COVID Mortality Growth Trends for Selected States"
    }
  }
  theData <- dataForMortalityPlots(countyChoices, movingAvg, stateChoices)

  assembleGrowthTrendPlot(theData, chooseCounty,
                          countyChoices,
                          stateChoices,
                          title,
                          paste("Last", timeWindow, "days"),
                          "Daily growth rate: new day's deaths as percent of previous total deaths",
                          timeWindow)
}

# Evaluation HTML -- stuff for summary page

source("./state_abbrev_lookup.R")

library(tidyverse)

statsFromDataAndKey <- function(sevenDayAvgNewData,
                                sevenDayAvgTotalData,
                                theLocaleKeys,
                                localePopulation,
                                doTrace = FALSE) {
  locale7DayAvgNewData  <- sevenDayAvgNewData %>%
    filter(Combined_Key == theLocaleKeys$spaced)

  jLDims <- dim(locale7DayAvgNewData)  
  locLen <- jLDims[2]

  # I observed an empty "locale7DayAvgNewData" data set for "Kenai Peninsula, AK, US",
  # once, therefore am defending against an empty data set. However,
  # "Kenai Peninsula,AK,US" is there. That's the reason I am now computing
  # spaced (e.g. "Kenai Peninsula, AK, US" and spaceless
  # (e.g. "Kenai Peninsula,AK,US") for theLocaleKeys. There are about 20
  # spaceless Combined_Key entries in the data as of Aug. 1, 2020.

  if (jLDims[1] == 0) {
    locale7DayAvgNewData  <- sevenDayAvgNewData %>%
      filter(Combined_Key == theLocaleKeys$spaceless)
    
    jLDims <- dim(locale7DayAvgNewData)  
    locLen <- jLDims[2]
  }
  
  if (jLDims[1] >= 1) {
    # Number of new cases per day, 7 day moving average
    lastAvgGrowthPer100K <- locale7DayAvgNewData %>%
      select(last_col()) * 100000 / localePopulation
    
    locale7DayAvgTotalData <- sevenDayAvgTotalData %>%
      filter(Combined_Key == theLocaleKeys$spaced)
    jLDims <- dim(locale7DayAvgTotalData)  
    locLen <- jLDims[2]
    if (jLDims[1] == 0) {
      locale7DayAvgTotalData <- sevenDayAvgTotalData %>%
        filter(Combined_Key == theLocaleKeys$spaceless)
        
      jLDims <- dim(locale7DayAvgTotalData)  
      locLen <- jLDims[2]
    }
      
    aVec <- as.vector(select(locale7DayAvgTotalData,
                             last_col(7):last_col()))
      
    pars <- leastSquaresTrendParams(1:7, as.numeric(aVec[2:8]))
      
    trendGrowth <- pars$slope * 100 / as.numeric(aVec[8])
      
    nNow           = as.data.frame(locale7DayAvgTotalData)[1,locLen]
    nPrevWk        = as.data.frame(locale7DayAvgTotalData)[1,locLen-7]
      
    if (doTrace) {  
      print(paste("Last week data for ", theLocaleKeys$spaced,
                  "/", theLocaleKeys$spaceless, sep=""))
      print(paste("  population: ", localePopulation, sep=""))
      print("Raw or moving average growth data:")
      print(as.data.frame(aVec)[1,2:8])
        
      print("Trend parameters")
      print(paste("  slope = ",
                  format(pars$slope, digits=4),
                  ", intercept = ",
                  format(pars$intercept, digits=4),
                  sep=""))
        
      print(paste("  slope * 100 / last_days_growth:",
                  format(trendGrowth, digits=4), sep=" "))
    }
  } else {
    trendGrowth          <- NA
    nNow                 <- NA
    nPrevWk              <- NA
    lastAvgGrowthPer100K <- NA
  }

  list(localeData     = locale7DayAvgNewData,
       growthPctTrend = trendGrowth,
       nNow           = nNow,
       nPrevWk        = nPrevWk,
       lastPer100K    = as.numeric(lastAvgGrowthPer100K))
}
  
computeStatsOfLocale <- function(aCounty, stateAbbrev,
                                 doTrace = FALSE) {
  haveStateOrUSData <-  FALSE
  theLocaleKeys <- makeCombinedKeys(aCounty, stateAbbrev)

  if ((!is.null(aCounty)) && (!is.na(aCounty)) && (aCounty != "")) {
    confirmedCaseNewAvg   <- US_County_Confirmed_NewAvg
    confirmedCaseTotalAvg <- US_County_Confirmed_Avg
    deathsNewAvg          <- US_County_Deaths_NewAvg
    deathsTotalAvg        <- US_County_Deaths_Avg
  } else {
    if ((!is.null(stateAbbrev)) && (!is.na(stateAbbrev)) && (stateAbbrev != "")) {
      confirmedCaseNewAvg   <- US_State_Confirmed_NewAvg
      confirmedCaseTotalAvg <- US_State_Confirmed_Avg
      deathsNewAvg          <- US_State_Deaths_NewAvg
      deathsTotalAvg        <- US_State_Deaths_Avg
      
      incidentRateData      <- US_State_Incident_Rate_NewAvg
      haveStateOrUSData     <- TRUE
    } else {
      confirmedCaseNewAvg   <- US_Confirmed_NewAvg
      confirmedCaseTotalAvg <- US_Confirmed_Avg
      deathsNewAvg          <- US_Deaths_NewAvg
      deathsTotalAvg        <- US_Deaths_Avg
      
      incidentRateData      <- US_Incident_Rate_NewAvg
      haveStateOrUSData <- TRUE
    }
  }

  if (doTrace) {
    print(paste("  localeKey spaced:", theLocaleKeys$spaced,
                "spaceless:", theLocaleKeys$spaceless))
  }
  
  localePopulationRecord <- filter(US_Population,
                                   Combined_Key == theLocaleKeys$spaced)
  lpDims <- dim(localePopulationRecord)
  if (lpDims[1] == 0) {
    localePopulationRecord <- filter(US_Population,
                                     Combined_Key == theLocaleKeys$spaceless)
    lpDims <- dim(localePopulationRecord)
  }
  if (doTrace) {
    print("  localePopulationRecord:")
    print(paste("    Province_State", localePopulationRecord$Province_State))
    print(paste("    Combined_Key", localePopulationRecord$Combined_Key))
    print(paste("    CountyName  ", localePopulationRecord$CountyName))
  }

  if (lpDims[1] > 0) {
    localePopulation <- localePopulationRecord$Population

    confirmedGrowth <- statsFromDataAndKey(confirmedCaseNewAvg,
                                           confirmedCaseTotalAvg,
                                           theLocaleKeys,
                                           localePopulation)
    
    growthRateConf  <- confirmedGrowth$lastPer100K
    growthTrendConf <- confirmedGrowth$growthPctTrend
    
    deathStats <- statsFromDataAndKey(deathsNewAvg,
                                      deathsTotalAvg,
                                      theLocaleKeys,
                                      localePopulation)
    
    # Number of deaths per day, 7 day moving average
    # = deathStats$lastPer100K
    # Estimate new cases from deaths
    growthRateEst  <- deathStats$lastPer100K * 100
    growthTrendEst <- deathStats$growthPctTrend
  } else {
    localePopulation <- NA
    confirmedGrowth  <- list(localeData     = NA,
                             growthPctTrend = NA,
                             nNow           = NA,
                             nPrevWk        = NA,
                             lastPer100K    = NA)
    growthRateConf    <- NA
    growthTrendConf   <- NA
    deathStats        <- list(localeData = NA,
                              nNow       = NA,
                              nPrevWk    = NA)
    growthRateEst     <- NA
    growthTrendEst    <- NA
  }

  list(popRecord    = localePopulationRecord,

       confGroDat   = confirmedGrowth$localeData,
       confGroRte   = growthRateConf,
       confGroTrend = growthTrendConf,
       confGroNow   = confirmedGrowth$nNow,
       confGroWkD   = (confirmedGrowth$nNow - 
                       confirmedGrowth$nPrevWk),

       estGroDat    = deathStats$localeData,
       estGroRte    = growthRateEst,
       estGroTrend  = growthTrendEst,
       estGroNow    = deathStats$nNow,
       estGroWkD    = (deathStats$nNow - deathStats$nPrevWk),

       HaveMore     = haveStateOrUSData
       )
}

ratingFromStatistic <- function(theStat, cutoffs, whatStat, doTrace = FALSE) {
  rating <- 1
  if (doTrace) {
    print(paste(whatStat, "rating was initialized to 1"))
  }
  if (is.na(theStat) || is.nan(theStat)) {
    rating <- 5
    if (doTrace) {
      print("theStat is not available, rating set to 5")
    }
  } else { 
    for (i in 1:3) {
      if (theStat > cutoffs[i]) {
        rating <- i + 1
        if (doTrace) {
          print(paste("rating was set to", rating))
        }
      }
    }
  }
  rating
}

validTrendString <- function(datum, whatDoesItMean) {
  if (is.nan(datum) || is.infinite(datum)) {
    trendString <- paste("Growth trend of ", whatDoesItMean,
                         " CAN'T BE COMPUTED", sep="")
  } else if (is.na(datum)) {
    trendString <- paste("Growth trend of ", whatDoesItMean,
                         " IS NOT AVAILABLE", sep="")
  } else {
    trendString <- paste("Growth trend of ", whatDoesItMean,
                         ": ",
                         format(datum, digits=3),
                         "% per day",
                         sep="")
  }
  trendString
}

evaluationOfLocale <- function(aCounty, stateAbbrev, doTrace = FALSE) {
  # class is one of the following
  classes <- c("hghi_green", "hghi_yellow", "hghi_orange", "hghi_red", "hghi_unused")
  
  # class is based on
  #
  # 1. Confirmed case incidence, new cases per 100000 population, 7 day moving average
  # 2. Confirmed case incidence trend
  # 3. Case incidence estimated as 100 * number of deaths
  # 4. Case incidence trend estimated as above

  caseConfRate <- c("fewer than 1", "1 to 10", "10 to 25", "over 25")
  caseIncTrend  <- c("flat or declining", "below 1% growth",
                     "1 to 3% growth", "over 3% growth")

  theCombinedKeys <- makeCombinedKeys(aCounty, stateAbbrev)

  Cutoff <- data.frame(Growth = c(1.0, 10.0, 25.0),
                       Trend  = c(0.0,  1.0,  3.0))
  
  incData <- computeStatsOfLocale(aCounty, stateAbbrev, doTrace)

  if (is.null(dim(incData$popRecord))) {
    ""
  } else {
    # By now you have a data frame to look up the relevant statistics in
    if (doTrace) {
      print(paste("NEW LOCALE", incData$popRecord$CountyName))
    }
    
    caseIncidenceTxt <- paste("Confirmed new cases: ",
                              format(incData$confGroRte, digits=2),
                              " daily per 100,000 population",
                              sep = "")
    CIC <- ratingFromStatistic(incData$confGroRte, Cutoff$Growth, "Confirmed case growth", doTrace)
    
    caseTrendTxt <- validTrendString(incData$confGroTrend, "confirmed new cases")
    CIT <- ratingFromStatistic(incData$confGroTrend, Cutoff$Trend,  "Confirmed case trend", doTrace)
    
    estIncidenceTxt <- paste("Estimated new cases: ",
                             format(incData$estGroRte, digits=2),
                             " daily per 100,000 population",
                             sep = "")
    EIC <- ratingFromStatistic(incData$estGroRte, Cutoff$Growth, "Estimated case growth", doTrace)
    
    estTrendTxt <- validTrendString(incData$estGroTrend, "estimated new cases")
    EIT <- ratingFromStatistic(incData$estGroTrend,  Cutoff$Trend,  "Estimated case trend", doTrace)
    
    overall <- 1
    for(aRating in c(CIC, CIT, EIC, EIT)) {
      if (aRating < 5) {
        overall <- max(overall, aRating)
      }
    }

    caseIncLI   <- as.character(tags$li(caseIncidenceTxt, class=classes[CIC]))
    caseTrendLI <- as.character(tags$li(caseTrendTxt,     class=classes[CIT]))
    estIncLI    <- as.character(tags$li(estIncidenceTxt,  class=classes[EIC]))
    estTrendLI  <- as.character(tags$li(estTrendTxt,      class=classes[EIT]))
    
    commonPart <- paste(caseIncLI, caseTrendLI, estIncLI, estTrendLI,
                        sep="")
    
    lowPopPart <- ""
    
    if ((dim(incData$popRecord)[1] > 0) && (incData$popRecord$Population < 50000)) {
      lowPopPart <- as.character(tags$li(paste("Predictions based on 7-day averages of",
                                               format(incData$confGroNow, digits=2),
                                               "confirmed cases and",
                                               format(incData$confGroWkD, digits=2),
                                               "new cases this week in a population of about",
                                               incData$popRecord$Population,
                                               "are not very reliable."),
                                         class=classes[5]))
    }

    as.character(tags$div(tags$h4(incData$popRecord$CountyName),
                          tags$ul(HTML(paste(commonPart,
                                             lowPopPart,
                                             sep=""))),
                          class=classes[overall]))
  }
}

#Call: summaryHTMLCounties(input$countyChoices, input$stateChoices[1])
summaryHTMLCounties <- function(countyChoices, stateAbbrev) {
  theText <- ""
  for (aCounty in countyChoices) {
    theText <- paste(theText,
                     evaluationOfLocale(aCounty, stateAbbrev),
                     sep="")
  }
  HTML(theText)
}

summaryHTMLStates <- function(stateChoices) {
  theText <- ""
  for (stateAbbrev in stateChoices) {
    theText <- paste(theText,
                     evaluationOfLocale(NA, stateAbbrev, doTrace = FALSE),
                     sep="")
  }
  HTML(theText)
}

summaryHTMLUS <- function() {
  HTML(evaluationOfLocale(NA, NA))
}

summaryHTMLAny <- function(chooseCounty, countyChoices, stateChoices) {
  DisabledForDebugging = FALSE
  if (DisabledForDebugging) {
    tags$h2("Sorry, this section is temporarily disabled for debugging")
  } else {
    if (!is_null(stateChoices)) {
      if (chooseCounty && !is_null(countyChoices)) {
        summaryHTMLCounties(countyChoices, stateChoices[1])
      } else {
        summaryHTMLStates(stateChoices)
      }
    } else {
      summaryHTMLUS()
    }
  }
}


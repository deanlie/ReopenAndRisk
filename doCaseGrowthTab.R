source("./loadUSConfirmedData.R")
source("./state_abbrev_lookup.R")
source("./assemblePlotObject.R")

dataForCaseGrowthPlots <- function(countyChoices, movingAvg, stateChoices) {
  if (is.null(stateChoices)) {
    if (movingAvg) {
      theData <- US_Confirmed_A7
    } else {
      theData <- US_Confirmed
    }
  } else {
    if (is.null(countyChoices)) {
      if (movingAvg) {
        theData <- US_State_Confirmed_A7
      } else {
        theData <- US_State_Confirmed
      }
    } else {
      if (movingAvg) {
        dataTibble <- US_County_Confirmed_A7
      } else {
        dataTibble <- US_County_Confirmed
      }
      theData <- dataTibble %>%
        filter(Province_State == stateLookup[stateChoices[1]])
    }
  }
  theData
}

caseHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
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
  pgphThree <- paste("It's possible that lines for some areas will be missing,
                     especially if you've selected some counties with low population.
                     That happens because a growth rate starting from zero
                     can't be computed. If you see that, please change the selection
                     to have ONLY the missing areas, and I'll display the NUMBER of
                     cases rather than the growth rate.")
  
  pgphFour <- paste("On August 6, the Council of State and Territorial Epidemiologists
                    announced the release of an updated COVID-19 national case
                    definition recently endorsed by CDC. States may have begun
                    using this new definition on different dates, and this may account
                    for sudden drops in the number of reported confirmed cases or
                    reported COVID-19 deaths. For instance, Massachusetts reported
                    confirmed cases dropped by 7636 cases between Sept. 2 and Sept. 3,
                    according to the numbers used by this web site.")

  theText <- paste(tags$h4("Trends of new cases"),
                   tags$p("The CDC's recommendation was that a state not begin reopening
                   after the initial lockdown until it had a downward trajectory or
                   near-zero incidence of documented cases over a 14 day period.
                   The trend of cases is still an important measure."),
                   tags$p(pgphTwo),
                   tags$p(pgphThree),
                   tags$p(pgphFour),
                   tags$p(),
                   sep="")
  HTML(theText)
}

plotCaseGrowthBoxplots <- function(chooseCounty,
                                   movingAvg,
                                   countyChoices,
                                   stateChoices,
                                   timeWindow) {
  if (is.null(stateChoices)) {
    theData <- dataForCaseGrowthPlots(NULL, movingAvg, "AK")
  } else {
    theData <- dataForCaseGrowthPlots(countyChoices, movingAvg, stateChoices)
  }
  
  if (movingAvg) {
    title <- "COVID Case Growth Distribution, 7 day moving average"
  } else {
    title <- "COVID Case Growth Distribution"
  }
  
  assembleGrowthBoxPlot(theData, chooseCounty,
                        countyChoices, stateChoices,
                        title,
                        paste("Last", timeWindow, "days"),
                        "Daily growth rate: new day's cases as percent of previous total cases",
                        clampFactor = 3, timeWindow = timeWindow)
}

plotCaseGrowthTrend <- function(chooseCounty,
                                movingAvg,
                                countyChoices,
                                stateChoices,
                                timeWindow) {
  if (is.null(stateChoices)) {
    title <- "COVID Case Growth Trend for US as a whole"
  } else {
    if (chooseCounty && !(is.null(countyChoices))) {
      title <- paste("COVID Case Growth Trends for Selected",
                     admin1TypeFor(stateChoices[1])$UC_PL)
    } else {
      title <- "COVID Case Growth Trends for Selected States"
    }
  }
  theData <- dataForCaseGrowthPlots(countyChoices, movingAvg, stateChoices)

  assembleGrowthTrendPlot(theData, chooseCounty,
                          countyChoices, stateChoices,
                          title,
                          paste("Last", timeWindow, "days"),
                          "Daily growth rate: new day's cases as percent of previous total cases",
                          timeWindow)
}

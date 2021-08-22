source("latestVaccExtremes.R")

vaccHeaderHTML <- function(movingAvg, vaccChoice) {
  if (movingAvg) {
    tooMuchData <- US_State_Vaccination_Pcts_A7
  } else {
    tooMuchData <- US_State_Vaccination_Pcts
  }
  
  nMin <- 3
  nMax <- 3
  extremaStates <- latestVaccExtremes(tooMuchData, vaccChoice, nMin, nMax)
  
  theMaxPct <- format(as.double(extremaStates$bot[nMax, 2]), digits=3)
  max2Pct   <- format(as.double(extremaStates$bot[nMax - 1, 2]), digits=3)
  theMinPct <- format(as.double(extremaStates$top[1, 2]), digits=3)
  min2Pct   <- format(as.double(extremaStates$top[2, 2]), digits=3)
  theText <- paste(tags$h4(paste("Vaccination", vaccChoice, "Data")),
                   tags$p("Vaccination data is shown by percent of state or of US as a whole."),
                   tags$p(paste("Highest ", vaccChoice, " rate: ",
                                extremaStates$bot[nMax, 1],
                                " with ", theMaxPct,
                                " percent", sep = "")),
                   tags$p(paste("Next highest rate: ",
                                extremaStates$bot[nMax - 1, 1],
                                " with ", max2Pct,
                                " percent", sep = "")),
                   tags$p(paste("Lowest ", vaccChoice, " rate: ",
                                extremaStates$top[1, 1],
                                " with ", theMinPct,
                                " percent", sep = "")),
                   tags$p(paste("Next lowest rate: ",
                                extremaStates$top[2, 1],
                                " with ", min2Pct,
                                " percent", sep = "")),
                   tags$p("Note that 'Total Doses' will be above 100% when close to 50% of the population
                            has had a second dose!"),
                   sep="")
  
  HTML(theText)
}

vaccRBoxHTML <- function(movingAvg, vaccChoice) {
}

vaccRTrendHTML <- function(movingAvg, vaccChoice) {
}

plotVaccBoxplots <- function(movingAvg, vaccChoice, stateChoices, timeWindow) {
  if (movingAvg) {
    title <- paste("Vaccination", vaccChoice, "State Distribution, 7 day moving average")
    tooMuchData <- US_State_Vaccination_Pcts_A7
  } else {
    title <- paste("Vaccination", vaccChoice, "State Distribution")
    tooMuchData <- US_State_Vaccination_Pcts
  }
  
  theData <- makeFullyVaccDataIfNeeded(tooMuchData, vaccChoice)
  
  timeWindow = min(timeWindow, dim(theData)[2] - 4)
  
  vaccTrendData <<- list(full=tooMuchData, filtered=theData)
  
  assembleDirectBoxPlot(theData, FALSE,
                        c(""),
                        stateChoices,
                        title,
                        paste("Last", timeWindow, "days"),
                        "Daily vaccination growth",
                        clampFactor = 3, timeWindow = timeWindow)
}

plotVaccTrend <- function(movingAvg, vaccChoice, stateChoices, timeWindow) {
  if (movingAvg) {
    if (length(stateChoices) > 0) {
      title <- paste("Vaccination", vaccChoice, "For Selected States, 7 day moving average")
      tooMuchData <- US_State_Vaccination_Pcts_A7
    } else {
      title <- paste("Vaccination", vaccChoice, "US Overall, 7 day moving average")
      tooMuchData <- US_Vaccination_Pcts_A7
    }
  } else {
    if (length(stateChoices) > 0) {
      title <- paste("Vaccination", vaccChoice, "For Selected States")
      tooMuchData <- US_State_Vaccination_Pcts
    } else {
      title <- paste("Vaccination", vaccChoice, "US Overall")
      tooMuchData <- US_Vaccination_Pcts
    }
  }
  
  theData <- makeFullyVaccDataIfNeeded(tooMuchData, vaccChoice)
  
  vaccTrendData <<- list(full=tooMuchData, filtered=theData)
  
  timeWindow = min(timeWindow, dim(theData)[2] - 4)
  
  assembleDirectTrendPlot(theData, FALSE,
                          NULL,
                          stateChoices,
                          title,
                          paste("Last", timeWindow, "days"),
                          "Daily vaccination percentages",
                          timeWindow = timeWindow, nFirst = 3,
                          tibbleName = "from plotVaccTrend")
}

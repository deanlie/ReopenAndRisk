# Test for plot palette

testPlotPalette <- function(theData, stateChoices, title, xLabels, yLabels, timeWindow,
                            traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testPlotPalette\n")
  }
  
  result <- assembleDirectBoxPlot(theData, FALSE,
                                  c(""),
                                  stateChoices,
                                  title,
                                  vaccPlotXLabels(timeWindow),
                                  vaccPlotYLabels(),
                                  clampFactor = 3, timeWindow = timeWindow,
                                  traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testPlotPalette\n")
  }

  return(result)
}

testSuite <- function(traceThisRoutine = FALSE, prepend = "TEST") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testSuite\n")
  }
  
  theData <- read_csv("DATA/CACHE/Test_Vacc_Bx.csv")
  stateChoices <- c("MA", "ME")
  title <- "Vaccination People Fully Vaccinated State Distribution, 7 day moving average"
  xLabels <- "Last 14 days"
  yLabels <- "Daily vaccination percentages"
  timeWindow <- 14

  result <- testPlotPalette(theData, stateChoices, title, xLabels, yLabels,
                            timeWindow, traceThisRoutine = traceThisRoutine,
                            prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testSuite\n")
  }
  return(result)
}

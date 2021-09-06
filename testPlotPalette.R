# Test for plot palette

source("columnUtilities.R")

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

testSuite0 <- function(traceThisRoutine = FALSE, prepend = "TEST") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testSuite0\n")
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
    cat(file = stderr(), prepend, "Leaving testSuite0\n")
  }
  return(result)
}

###########################################
#     Routine Under Test                  #
###########################################

testSuite1 <- function(traceThisRoutine = FALSE, prepend = "TEST") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testSuite1\n")
  }
  
  theData <- read_csv("DATA/US_State_Confirmed.csv",
                     col_types = myTSColTypes())
  result1 <- theData %>%
    returnEndsOfTibbleRow(itsName = "US_State_Confirmed",
                          theKey = "Combined_Key",
                          keyValue = "Massachusetts, US",
                          nFirst = 2, nLast = 8,
                          optionalMessage = "",
                          traceThisRoutine = TRUE,
                          prepend = "")
  
  result2 <- theData %>%
    returnEndsOfTibbleRow(itsName = "US_State_Confirmed",
                          theKey = "Province_State",
                          keyValue = "Arizona",
                          nFirst = 4, nLast = 3,
                          optionalMessage = "",
                          traceThisRoutine = TRUE,
                          prepend = "")

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testSuite1\n")
  }
  return(result1)
}

testSuite <- function(traceThisRoutine = FALSE, prepend = "TEST") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered testSuite\n")
  }
  
  loadUSIncidentRateData(traceThisRoutine = TRUE)
  
  foo <- conciseEndsOfTibbleRow(US_Incident_Rate, itsName = "US_Incident_Rate",
                                keyValue = "US", nLast = 3,
                                traceThisRoutine = TRUE)
  foo <- conciseEndsOfTibbleRow(US_Incident_Rate_G7, itsName = "US_Incident_Rate_G7",
                                keyValue = "US", nLast = 3,
                                traceThisRoutine = TRUE)

  result1 <- loadUSIncidentRateData1(traceThisRoutine = TRUE)
  
  foo <- conciseEndsOfTibbleRow(result1$US_C, itsName = "result1$US_C",
                                keyValue = "US", nLast = 3,
                                traceThisRoutine = TRUE)
  foo <- conciseEndsOfTibbleRow(result1$US_C_A, itsName = "result1$US_C_A",
                                keyValue = "Arizona, US", nLast = 3,
                                traceThisRoutine = TRUE)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving testSuite\n")
  }
  return(result1)
}


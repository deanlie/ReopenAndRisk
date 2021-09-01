source("columnUtilities.R")

# existingST <- rebuildStateDataFilesForTypes(nDates = 10, stopNDaysBeforePresent = 5)
# 
# limitedST <- discardOutdatedDataFromStateTibbles(existingST, keepUpToNDaysBeforePresent = 12)
# 
# dropCols <- limitedST
# dropCols$Testing_Rate <- select(dropCols$Testing_Rate, -`7/17/2021`)
# dropCols$Incident_Rate <- select(dropCols$Incident_Rate, -`7/18/2021`, -`7/20/2021`)
# foo <- uniformizeDatesOfStateTibbles(dropCols)
# 
# updateExistingStateDataFilesForTypes(limitedST,
#                                      nDates = 12,
#                                      stopNDaysBeforePresent = 0,
#                                      traceThisRoutine = TRUE,
#                                      prepend = "Console!")
# 
# nn <- names(US_State_Confirmed)
# num_nn <- nn[grep("^[0-9]+/[0-9]+/[0-9]+$", nn)]
# num_nn <- nn[grep("^1?[0-9]/[1-3]?[0-9]/2[0-9]$", nn)]
# nonnum_nn <- nn[grep("^1?[0-9]/[1-3]?[0-9]/2[0-9]$", nn, invert = TRUE)]

test_Drops <- function(traceThisRoutine = FALSE) {
  US_CD <- list(Confirmed = read_csv("./DATA/US_Confirmed.csv",
                                     col_types = myTSColTypes()),
                Deaths = read_csv("./DATA/US_Deaths.csv",
                                  col_types = myTSColTypes()))
  US_TICR <- list(Total_Test_Results = read_csv("../ReopenAndRisk/DATA/US_Total_Test_Results.csv",
                                                col_types = justCKColTypes())
  )

  # Testing_Rate = read_csv("../ReopenAndRisk/DATA/US_Testing_Rate.csv",
  #                               col_types = justCKColTypes())
  # Incident_Rate = read_csv("../ReopenAndRisk/DATA/US_Incident_Rate.csv",
  #                               col_types = justCKColTypes())
  # Case_Fatality_Ratio = read_csv("../ReopenAndRisk/DATA/US_Case_Fatality_Ratio.csv",
  #                               col_types = justCKColTypes())
  
  foo <- discardOlderDataFromListOfTibbles(US_CD,
                                           keepFromNDaysBeforePresent = 18,
                                           traceThisRoutine = traceThisRoutine)
  
  traceThisRoutine <- TRUE  
  boo_LT <- discardOlderDataFromListOfTibbles(US_TICR,
                                              keepFromNDaysBeforePresent = 20,
                                              traceThisRoutine = traceThisRoutine)
  traceThisRoutine <- FALSE  
  boo_ST <- discardOutdatedDataFromStateTibbles(US_TICR,
                                                keepUpToNDaysBeforePresent = 20,
                                                traceThisRoutine = traceThisRoutine)
  
  return(list(TTR = list(LT = boo_LT$Total_Test_Results, ST = boo_ST$Total_Test_Results),
              TR = list(LT = boo_LT$Testing_Rate, ST = boo_ST$Testing_Rate),
              IR = list(LT = boo_LT$Incident_Rate, ST = boo_ST$Incident_Rate),
              CFR = list(LT = boo_LT$Case_Fatality_Ratio, ST = boo_ST$Case_Fatality_Ratio)))
  
}

test_BuildUS <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), "Entered test_BuildUS\n")
  }
  
  stateTibbles <- openExistingStateTibbles(traceThisRoutine = FALSE)

  USTibbles <- rebuildUSDataFilesForTypes(stateTibbles, traceThisRoutine = traceThisRoutine)
  
  # OUCH set FALSE after debugging
  traceThisRoutine = TRUE
  if (traceThisRoutine) {
    dumpTibbleStart(USTibbles$USTTR, "USTTR")
    
    dumpTibbleStart(USTibbles$US_CFR_0, "US_CFR_0")
    dumpTibbleStart(USTibbles$US_CFR,   "US_CFR")
    
    dumpTibbleStart(USTibbles$US_IR_0, "US_IR_0")
    dumpTibbleStart(USTibbles$US_IR,   "US_IR")
    
    dumpTibbleStart(USTibbles$US_TR_0, "US_TR_0")
    dumpTibbleStart(USTibbles$US_TR,   "US_TR")
  }
  
  return(list(State = stateTibbles, US = USTibbles))
}

testTRvsTR0 <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), "Entered testTRvsTR0\n")
  }
  
  # NOTE! Discrepancy between the _0's("AsWeightedAverage" and the
  #   not _0 is difference between the total US population from US_Population.csv
  #   and the number estimated by summing state estimates. I need to save the population
  #   estimates, which I can then use instead of US_Population.csv if I want consistent
  #   results. Or, look, add a column to US_Population.csv and use that. But a separate
  #   table, computed daily, is cleaner

  US_Total_Test_Results <- read_csv("./DATA/US_Total_Test_Results.csv",
                                    col_types = justCKColTypes())
  US_State_Testing_Rate <- read_csv("./DATA/US_State_Testing_Rate.csv",
                                    col_types = justCKColTypes())
  
  US_Testing_Rate_0 <- rebuildUSDataFileForTypeAsWeightedAvg(US_State_Testing_Rate,
                                                             "Testing_Rate",
                                                             traceThisRoutine = traceThisRoutine,
                                                             prepend = myPrepend)
  US_Testing_Rate <- rebuildUSDataFileForTypeByNormalizing(US_Total_Test_Results,
                                                           "Testing_Rate",
                                                           traceThisRoutine = traceThisRoutine,
                                                           prepend = myPrepend)

  if (traceThisRoutine) {
    dumpTibbleStart(US_Total_Test_Results, "TTR", prepend = "  ")
    dumpTibbleStart(US_Testing_Rate_0$IM, "TR_0_i", prepend = "  ")
    dumpTibbleStart(US_Testing_Rate_0$FF, "TR_0", prepend = "  ")
    dumpTibbleStart(US_Testing_Rate,   "TR", prepend = "  ")
    cat(file = stderr(), prepend, "Leaving testTRvsTR0\n")
  }
  
  return(list(US_TR_0 = US_Testing_Rate_0, US_TR = US_Testing_Rate))
}

testPopEstimate <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), "Entered testPopEstimate\n")
  }
  aDate <- Sys.Date() - 1
  dailyStateData <- read_csv(dataPathForDate(aDate),
                             col_types = dataFileColTypes())
  foo <- updatePopulationEstimateData(aDate, dailyStateData,
                                           traceThisRoutine = TRUE, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), "Leaving testPopEstimate\n")
  }
  return(foo)
}

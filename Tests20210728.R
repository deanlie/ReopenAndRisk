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
                                     col_types = cols(
                                       .default = col_double(),
                                       Province_State = col_logical(),
                                       Combined_Key = col_character()
                                     )),
                Deaths = read_csv("./DATA/US_Deaths.csv",
                                  col_types = cols(
                                    .default = col_double(),
                                    Province_State = col_logical(),
                                    Combined_Key = col_character())))
  US_TICR <- list(Total_Test_Results = read_csv("../ReopenAndRisk/DATA/US_Total_Test_Results.csv",
                                                col_types = cols(
                                                  .default = col_double(),
                                                  Combined_Key = col_character()
                                                ))
  )
                  # ,
                  # Testing_Rate = read_csv("../ReopenAndRisk/DATA/US_Testing_Rate.csv",
                  #                               col_types = cols(
                  #                                 .default = col_double(),
                  #                                 Combined_Key = col_character()
                  #                               )),
                  # Incident_Rate = read_csv("../ReopenAndRisk/DATA/US_Incident_Rate.csv",
                  #                               col_types = cols(
                  #                                 .default = col_double(),
                  #                                 Combined_Key = col_character()
                  #                               )),
                  # Case_Fatality_Ratio = read_csv("../ReopenAndRisk/DATA/US_Case_Fatality_Ratio.csv",
                  #                               col_types = cols(
                  #                                 .default = col_double(),
                  #                                 Combined_Key = col_character())))
  
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
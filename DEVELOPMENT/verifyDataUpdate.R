library(tidyverse)

source("dateFormatRoutines.R")

# ./DATA/STATIC/VariousEnds to have data for testing of update routines;
#     that is, data limited to few states & counties & dates, but not all the
#     same end dates; not through Shinytest, but using routine testDataUpdate
#     at the bottom of this module
# Update routines are to produce data limited to those states & counties, all
#     with the same end date, in ./DATA/STATIC/, which can be verified by
#     comparison with a Shinytest snapshot

verifyUpdateOfListedFiles <- function(dateString) {
  desiredDate <- as.Date(dateString)
  files <- c("US_Case_Fatality_Ratio.csv",
             "US_Confirmed.csv",
             "US_County_Confirmed.csv",
             "US_County_Deaths.csv",
             "US_Deaths.csv",
             "US_Incident_Rate.csv",
             "US_State_Case_Fatality_Ratio.csv",
             "US_State_Confirmed.csv",
             "US_State_Deaths.csv",
             "US_State_Incident_Rate.csv",
             "US_State_Testing_Rate.csv",
             "US_State_Total_Test_Results.csv",
             "US_State_Vaccinations.csv",
             "US_Testing_Rate.csv",
             "US_Total_Test_Results.csv",
             "US_Vaccinations.csv")

  updateStatus <- verifyFileListLatestUpdates(files, desiredDate,
                                              "./DATA/STATIC/VariousEnds/")
  sortedStatus <- arrange(updateStatus, lastUpdate)
}

updateDataAndCleanUp <- function() {
  # filesNeedingDailyData <- c("US_Confirmed.csv",
  #                            "US_State_Confirmed.csv",
  #                            "US_County_Confirmed.csv",
  #                            "US_Deaths.csv",
  #                            "US_State_Deaths.csv",
  #                            "US_County_Deaths.csv")
  # filesNeedingTSData <- c("US_Incident_Rate.csv",
  #                         "US_State_Incident_Rate.csv",
  #                         "US_Case_Fatality_Ratio.csv",
  #                         "US_State_Case_Fatality_Ratio.csv",
  #                         "US_Total_Test_Results.csv",
  #                         "US_State_Total_Test_Results.csv",
  #                         "US_Testing_Rate.csv",
  #                         "US_State_Testing_Rate.csv",
  #                         "US_Vaccinations.csv",
  #                         "US_State_Vaccinations.csv",)
  # updateDataGroupAndCleanUp(filesNeedingDailyData, today - 1)
  # updateDataGroupAndCleanUp(filesNeedingTSData, today)
}

moveTestGroupFromVariousToStatic <- function(testGroup) {
  for (aFile in testGroup) {
    sourcePath <- paste("./DATA/STATIC/VariousEnds/", aFile, sep = "")
    destPath <- paste("./DATA/STATIC/", aFile, sep = "")
    system2("cp", c(sourcePath, destPath))
  }
} 

testDataUpdate <- function() {
  testGroup <- c("US_State_Confirmed.csv")
  testDate <- today - 1
  moveTestGroupFromVariousToStatic(testGroup)
  updateDataGroupAndCleanUp(testGroup, testDate)
  verifyFileListLatestUpdates(testGroup, testDate, "./DATA/")
}


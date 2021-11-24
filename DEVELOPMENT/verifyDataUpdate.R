library(tidyverse)

source("dateFormatRoutines.R")

verifyDataUpdate <- function(files, desiredDate) {
  # Get desired date into column header format
  # desiredLastColName <- formatDateForColumnName(desiredDate)
  desiredLastColName <- paste(month(desiredDate),
                              day(desiredDate),
                              (year(desiredDate) - 2000), sep="/")
 
  nErrors <- 0

  mismatches <- tibble()

  for (aFile in files) {
    # Read tibble
    aPath <- paste("./DATA/ClipDates/VariousEnds/", aFile, sep = "")
    aTibble <- read_csv(aPath, show_col_types = FALSE)
    
    # Get last column name
    theNames <- names(aTibble)
    lastName <- theNames[length(theNames)]
    
    # complain if it's not the date we want
    if (!identical(lastName, desiredLastColName)) {
      cat(file = stderr(), "MISMATCH in file ", aFile,
          " wanted ", desiredLastColName,
          " saw ", lastName, "\n")
      nErrors <- nErrors + 1
      mismatches[nErrors, "file"] = aFile
      mismatches[nErrors, "date"] = as.Date(lastName, format = "%m/%d/%y")
    }
  }

  earliestLastDate <- desiredDate + 1

  return(mismatches)
}

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

  updateStatus <- verifyDataUpdate(files, desiredDate)
  sortedStatus <- arrange(updateStatus, date)
}

updateDataAndCleanUp <- function() {
  filesNeedingDailyData <- c("US_Confirmed.csv",
                             "US_State_Confirmed.csv",
                             "US_County_Confirmed.csv",
                             "US_Confirmed.csv",
                             "US_State_Confirmed.csv",
                             "US_County_Confirmed.csv",
                             "US_Confirmed.csv",
                             "US_State_Confirmed.csv",
                             "US_County_Confirmed.csv")
  updateDataGroupAndCleanUp(filesNeedingDailyData, today - 1)
}
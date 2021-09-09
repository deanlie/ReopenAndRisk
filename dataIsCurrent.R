
library(tidyverse)
library(lubridate)
library(stringi)
library(RCurl)

source("URLFunctions.R")
source("dateFormatRoutines.R")
source("./mostRecentDataDate.R")

dataIsCurrent <- function(testDataPath) {
  desiredLatestDate <- expectedLatestUpdateDataDate()
  desiredLatestDateSlashes <- paste(month(desiredLatestDate),
                                    day(desiredLatestDate),
                                    (year(desiredLatestDate) - 2000), sep="/")
  
  # If ./DATA/US_<aType>.csv has desiredLatestDateSlashes as its last column name,
  #    it (and therefore presumably all the data with similar creation protocol)
  #    has been updated
  
  traceThisRoutine <- FALSE
  itIsCurrent <- FALSE
  
  options(show.error.messages = traceThisRoutine)
  if (traceThisRoutine) {
    print("before try(read_csv(testDataPath.. in dataIsCurrent")
  }
  testData <- try(read_csv(testDataPath, col_types=cols(.default = col_double(),
                                                        Province_State = col_logical(),
                                                        Combined_Key = col_character())))
  if (traceThisRoutine) {
    print("after try(read_csv(testDataPath.. in dataIsCurrent")
  }

  options(show.error.messages = TRUE)
  if (class(testData)[1] != "try-error") {
    # So far so good
    # The file exists. is it up to date?
    if (names(testData[dim(testData)[2]]) == desiredLatestDateSlashes) {
      # It is up to date. Return TRUE
      itIsCurrent <- TRUE
    } else {
      if (traceThisRoutine) {
        print(paste("Latest data for", testDataPath, "is", names(testData[dim(testData)[2]]), sep = " "))
      }
    }
  } else {
    if (traceThisRoutine) {
      print(paste("Unable to read", testDataPath, sep = " "))
    }
  }
  
  itIsCurrent
}

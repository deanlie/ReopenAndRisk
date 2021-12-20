library(tidyverse)
library(lubridate)
library(stringi)

verifyFileListLatestUpdates <- function(files, # Vector of strings
                                        desiredDate, # of class Date
                                        theDirectory = "./DATA/",
                                        traceThisRoutine = FALSE,
                                        prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered verifyFileListLatestUpdate\n")
  }
  
  # Get desired date into column header format
  desiredLastColName <- paste(month(desiredDate),
                              day(desiredDate),
                              (year(desiredDate) - 2000), sep="/")
  
  nErrors <- 0
  mismatches <- tibble()
  
  for (aFile in files) {
    # Read tibble
    aPath <- paste(theDirectory, aFile, sep = "")
    aTibble <- read_csv(aPath, show_col_types = FALSE)
    
    # Get last column name
    theNames <- names(aTibble)
    lastName <- theNames[length(theNames)]
    
    # complain if it's not the date we want
    if (!identical(lastName, desiredLastColName)) {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend,
            "MISMATCH in file ", aFile,
            " wanted ", desiredLastColName,
            " saw ", lastName, "\n")
      }
      nErrors <- nErrors + 1
      mismatches[nErrors, "fileName"] = aFile
      mismatches[nErrors, "lastUpdate"] = as.Date(lastName, format = "%m/%d/%y")
    }
  }
  
  if (dim(mismatches)[1] > 1) {  
    mismatches <- arrange(mismatches, lastUpdate)
  }
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving verifyFileListLatestUpdate\n")
  }
  
  return(mismatches)
}

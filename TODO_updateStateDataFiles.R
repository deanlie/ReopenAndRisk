
source("columnUtilities.R")

findFirstMissingDate <- function(aTibble, nFirst, nDates, lastDate,
                             traceThisRoutine = FALSE,
                             prepend = "") 
{
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered findfirstMissingDate\n")
  }

  firstDate <- lastDate - (nDates - 1)
  aDate <- firstDate
  colNames <- names(aTibble)
  while (aDate <= lastDate) {
    formattedDate <- formatDateForVaccColumnName(aDate)
    if (! (formattedDate %in% colNames)) {
      cat(file = stderr(), myPrepend, "First missing date:", formattedDate, "\n")
      break
    }
    aDate <- aDate + 1
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving findfirstMissingDate\n")
  }
  
  return(formattedDate)
}

testFFMD <- function()
{
  rawData <- read_csv("DATA/US_Vaccinations.csv",
                      col_types = vaccColTypes())
  
  D1 <- "8/13/21"
  D2 <- "8/19/21"
  D3 <- "8/22/21"
  
  filteredData <- select(rawData, Combined_Key, Datum, Loc_Datum, {D1}, {D2}, {D3})
  M1 <- findFirstMissingDate(filteredData, 3, 14, Sys.Date(), 
                         traceThisRoutine = TRUE, prepend = "Filtered")
  M2 <- findFirstMissingDate(rawData, 3, 14, Sys.Date(), 
                         traceThisRoutine = TRUE, prepend = "Raw     ")
  list(M1 = M1, M2 = M2)
}

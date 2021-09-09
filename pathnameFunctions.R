source("dateFormatRoutines.R")

filenameOfSansCountyDataForDate <- function(aDate) {
  paste(jhuFileDateString(aDate), ".csv", sep="")
}

pathnameOfSansCountyUpdateDataForDate <- function(aDate) {
  paste("./DATA/",filenameOfSansCountyDataForDate(aDate), sep = "")
}

filenameOfStateLevelDataForDate <- function(aDate) {
  paste(jhuFileDateString(aDate), ".csv", sep="")
}

pathnameOfStateLevelUpdateDataForDate <- function(aDate) {
  paste("./DATA/",filenameOfStateLevelDataForDate(aDate), sep = "")
}

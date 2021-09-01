# The Johns Hopkins Github site readme file says,
#  "Since June 15, We are moving the update time forward to occur between
#   04:45 and 05:15 GMT to accommodate daily updates from India's 
#   Ministry of Health and Family Welfare."
# I am giving them 59 minutes grace before risking this program's failure.

library(lubridate)

expectedLatestUpdateDataDate <- function(UT_UpdateHour = 5) {
  UTCNow <- now("UTC")
  UTC_PXlt <- as.POSIXlt(UTCNow)
  if (UTC_PXlt$hour < UT_UpdateHour ||
      ((UTC_PXlt$hour == UT_UpdateHour) && (UTC_PXlt$min < 59))) {
    expectedDate <- as.Date(UTC_PXlt) - 2
  } else {
    expectedDate <- as.Date(UTC_PXlt) - 1
  }
  
  expectedDate
}

expectedLatestUpdateDataDateSlashes <- function() {
  desiredLatestDate <- expectedLatestUpdateDataDate()
  desiredLatestDateSlashes <- paste(month(desiredLatestDate),
                                    day(desiredLatestDate),
                                    (year(desiredLatestDate) - 2000), sep="/")
}

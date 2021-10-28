source("loadAllUSData.R")
source("mostRecentDataDate.R")
source("dateFormatRoutines.R")

library(tidyverse)

rebuildPeopleTestedData <- function(staticDataQ) {
  CountyTimeSeries_B <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/testing_data/county_time_series_covid19_US.csv",
                             show_col_types = FALSE) %>%
    select(combined_key, date, tests_combined_total)
  
  # Get date range ()
  nDaysData <- nDaysDataF(staticDataQ)
  
  if (staticDataQ) {
    aDate <- as.Date("2021-09-20")
  } else {
    aDate <- expectedLatestUpdateDataDate()
  }
  firstDate <- (aDate - nDaysData) + 1
}

filterStaticCountyData <- function(dateLimitedData) {
  outputData <- filter(dateLimitedData,
                       (Combined_Key == "Autauga, Alabama, US") |
                         (Combined_Key == "Natchitoches, Louisiana, US") |
                         (Combined_Key == "Plaquemines, Louisiana, US") |
                         (Combined_Key == "Arecibo, Puerto Rico, US") |
                         (Combined_Key == "Barceloneta, Puerto Rico, US") |
                         (Combined_Key == "Barnstable, Massachusetts, US") |
                         (Combined_Key == "Berkshire, Massachusetts, US") |
                         (Combined_Key == "Dukes, Massachusetts, US") |
                         (Combined_Key == "Middlesex, Massachusetts, US") |
                         (Combined_Key == "Nantucket, Massachusetts, US") |
                         (Combined_Key == "Suffolk, Massachusetts, US") |
                         (Combined_Key == "Worcester, Massachusetts, US") |
                         (Combined_Key == "Penobscot, Maine, US") |
                         (Combined_Key == "Broward, Florida, US") |
                         (Combined_Key == "Brazoria, Texas, US") |
                         (Combined_Key == "Deaf Smith, Texas, US") |
                         (Combined_Key == "Harris, Texas, US"))
  
}

countyTimeSeries_to_peopleTested <- function(staticDataQ) {
  # Get date range ()
  nDaysData <- nDaysDataF(staticDataQ)
  
  if (staticDataQ) {
    aDate <- as.Date("2021-09-20")
  } else {
    aDate <- expectedLatestUpdateDataDate()
  }
  firstDate <- (aDate - nDaysData) + 1
  
  CountyTimeSeries_B <- CountyTimeSeries %>%
    select(combined_key, date, tests_combined_total)

  buildingTibble <- CountyTimeSeries_B %>%
    filter(date == as.character(firstDate)) %>%
    select(combined_key, tests_combined_total)
  
  names(buildingTibble) <- c("Combined_Key",
                             formatDateForColumnName(firstDate)) 
  
  for (n in 1:(nDaysData - 1)) {
    cat(file = stderr(), "Before n =", n,
        "dim(buildingTibble)[1] =",
        dim(buildingTibble)[1], "\n")
    newDateTibble <- CountyTimeSeries_B %>%
      filter(date == as.character(firstDate + n)) %>%
      select(combined_key, tests_combined_total)
    
    names(newDateTibble) <- c("Combined_Key",
                               formatDateForColumnName(firstDate + n))
    
    buildingTibble <- left_join(buildingTibble, newDateTibble,
                                by = "Combined_Key")
    
    # print(as.character(firstDate + n,"%Y-%m-%d"))
  }
  
  if (staticDataQ) {
    buildingTibble <- filterStaticCountyData(buildingTibble)
  }

  return(buildingTibble)
}

howMuchCountyData <- function() {
  # Get date range ()
  nDaysData <- nDaysDataF(TRUE)
  
  aDate <- as.Date("2021-09-20")
  firstDate <- (aDate - nDaysData) + 1
  
  CountyTimeSeries_B <- CountyTimeSeries %>%
    select(combined_key, date, tests_combined_total)
  
  buildingTibble <- CountyTimeSeries_B %>%
    filter(date == as.character(firstDate)) %>%
    select(combined_key, tests_combined_total)
  
  names(buildingTibble) <- c("Combined_Key",
                             formatDateForColumnName(firstDate)) 
  
  buildingTibble <- buildingTibble %>%
    filterStaticCountyData()

  for (n in 1:(nDaysData - 1)) {
    cat(file = stderr(), "Before n =", n,
        "dim(buildingTibble)[1] =",
        dim(buildingTibble)[1], "\n")
    newDateTibble <- CountyTimeSeries_B %>%
      filter(date == as.character(firstDate + n)) %>%
      select(combined_key, tests_combined_total)
    
    names(newDateTibble) <- c("Combined_Key",
                              formatDateForColumnName(firstDate + n))
    
    buildingTibble <- buildingTibble %>%
      left_join(newDateTibble, by = "Combined_Key") %>%
      filterStaticCountyData()
    
    # print(as.character(firstDate + n,"%Y-%m-%d"))
  }
  cat(file = stderr(), "After loop dim(buildingTibble)[1] =",
      dim(buildingTibble)[1], "\n")
  
  return(buildingTibble)
}

#START
filterMAData <- function(aTibble) {
  outputData <- aTibble %>%
    filter(str_detect(combined_key, "Massachusetts"))
}
                         
howMuchMAData <- function() {
  # Get date range ()
  nDaysData <- nDaysDataF(TRUE)
  
  aDate <- as.Date("2021-09-20")
  firstDate <- (aDate - nDaysData) + 1
  
  myResult <- CountyTimeSeries %>%
    select(combined_key, date, tests_combined_total) %>%
    filterMAData()
  
  # buildingTibble <- CountyTimeSeries_B %>%
  #   filter(date == as.character(firstDate)) %>%
  #   select(combined_key, tests_combined_total)
  # 
  # names(buildingTibble) <- c("Combined_Key",
  #                            formatDateForColumnName(firstDate)) 
  # 
  # buildingTibble <- buildingTibble %>%
  #   filterMAData()
  # 
  # for (n in 1:(nDaysData - 1)) {
  #   cat(file = stderr(), "Before n =", n,
  #       "dim(buildingTibble)[1] =",
  #       dim(buildingTibble)[1], "\n")
  #   newDateTibble <- CountyTimeSeries_B %>%
  #     filter(date == as.character(firstDate + n)) %>%
  #     select(combined_key, tests_combined_total)
  #   
  #   names(newDateTibble) <- c("Combined_Key",
  #                             formatDateForColumnName(firstDate + n))
  #   
  #   buildingTibble <- buildingTibble %>%
  #     left_join(newDateTibble, by = "Combined_Key") %>%
  #     filterMAData()
  #   
  #   # print(as.character(firstDate + n,"%Y-%m-%d"))
  # }
  cat(file = stderr(), "After loop dim(myResult)[1] =",
      dim(myResult)[1], "\n")
  
  return(myResult)
}

makeStaticData <- function() {
  timeLimitedData <- countyTimeSeries_to_peopleTested(TRUE)
  write_csv(timeLimitedData,
            file = "./DATA/STATIC/US_County_Test_Positivity.csv")
}

makeTestResultWorksheet <- function() {
  formatNextDayForColumnName <- function(aString) {
    nextDay <- str_replace_all(as.character(as.Date(aString) + 1,
                                            format = "%m/%d/%y"),
                               "0([1-9])",
                               "\\1")
  }
  
  # Compute new on date
  USTestsToDate <- read_csv("./DATA/STATIC/US_Total_Test_Results.csv",
                            show_col_types = FALSE)
  NewerUSTests <- USTestsToDate[, 4:dim(USTestsToDate)[2]]
  OlderUSTests <- USTestsToDate[, 3:(dim(USTestsToDate)[2] - 1)]
  names(OlderUSTests) <- names(NewerUSTests)
  
  bind_cols(select(USTestsToDate, Province_State, Combined_Key),
            (NewerUSTests - OlderUSTests))          

  USConfirmedToDate <- read_csv("./DATA/STATIC/US_Confirmed.csv",
                                show_col_types = FALSE)
  NewerUSConfirmed <- USConfirmedToDate[, 4:dim(USConfirmedToDate)[2]]
  OlderUSConfirmed <- USConfirmedToDate[, 3:(dim(USConfirmedToDate)[2] - 1)]
  names(OlderUSConfirmed) <- names(NewerUSConfirmed)
  
  bind_cols(select(USConfirmedToDate, Province_State, Combined_Key),
            (NewerUSConfirmed - OlderUSConfirmed))          

  USTestsToYesterday <- USTestsToDate %>%
    select(-dim(USTestsToDate)[2])
  
  names(USTestsToYesterday) <- oldUSConfirmedColNames[c(1, 2, 4:(length(oldColNames)))]

  # NewerData <- theData[, 3:(2 + theRange$endColumn - theRange$startColumn)]
  # OlderData <- theData[, 2:(1 + theRange$endColumn - theRange$startColumn)]
  
  return(TestsToYesterday)
}

  
  
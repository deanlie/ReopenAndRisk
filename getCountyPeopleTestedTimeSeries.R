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
#STOP

makeStaticData <- function() {
  timeLimitedData <- countyTimeSeries_to_peopleTested(TRUE)
  write_csv(timeLimitedData,
            file = "./DATA/STATIC/US_County_Test_Positivity.csv")
}

  
  
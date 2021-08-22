# https://github.com/CSSEGISandData/COVID-19/blob/master/
#   csse_covid_19_data/csse_covid_19_daily_reports/
#   07-15-2021.csv

# "https://raw.githubusercontent.com/"
# "CSSEGISandData/COVID-19/master/"
# "csse_covid_19_data/"

# https://github.com/CSSEGISandData/COVID-19/blob/master/
#   csse_covid_19_data/csse_covid_19_daily_reports/
#   07-15-2021.csv

# https://raw.githubusercontent.com/
# CSSEGISandData/COVID-19/master/
# csse_covid_19_data/
# csse_covid_19_daily_reports/04-01-2021.csv

library(tidyverse)
# library(lubridate)
# library(stringi)
library(RCurl)

source("./dateFormatRoutines.R")

repositoryPathForDate <- function(aDate) {
  paste("https://raw.githubusercontent.com/",
        "CSSEGISandData/COVID-19/master/",
        "csse_covid_19_data/",
        "csse_covid_19_daily_reports/",
        jhuFileDateString(aDate), ".csv",
        sep = "")
}

saveJHUHereForDate <- function(aDate) {
  paste("F:/DeanDocuments/COVID_DATA/JHU/",
        jhuFileDateString(aDate), ".csv",
        sep = "")
}

processFileForDate <- function(aDate) {
  traceThisRoutine = TRUE
  # Download the data
   
  desiredURL <- repositoryPathForDate(aDate)

  # Get the data
  theColTypes <- cols(.default = col_double(),
                      Combined_Key = col_character(),
                      Province_State = col_character(),
                      Country_Region = col_character(),
                      Last_Update = col_datetime(format = ""))
  fullDaysData <- read_csv(desiredURL,
                           col_types = theColTypes)
  
  # Select and filter to get only desired data
  requiredData <- fullDaysData %>%
    select(Combined_Key, FIPS,
           Admin2, Province_State, Country_Region,
           Confirmed, Deaths, Incident_Rate, Case_Fatality_Ratio) %>%
    filter(!str_detect(Combined_Key, "Princess")) %>%
    filter(!str_detect(Combined_Key, "Unassigned")) %>%
    filter(str_detect(Combined_Key, "US"))
  
  # Save the desired data
  savePath <- saveJHUHereForDate(aDate)
  
  write_csv(requiredData, savePath)
  
  print(paste("OK, we downloaded", desiredURL,
              "and wrote", savePath))
}

firstDate <- as.Date("2021-04-01")
processFileForDate(firstDate)

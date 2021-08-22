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

# https://raw.githubusercontent.com/"
# "govex/COVID-19/master/"
# "data_tables/vaccine_data/us_data/hourly/vaccine_data_us.csv

library(tidyverse)
# library(lubridate)
# library(stringi)
library(RCurl)

source("./dateFormatRoutines.R")

GithubUserContent <- function() {
  return("https://raw.githubusercontent.com/")
}

CSSEGICOVID19Master <- function() {
  return("CSSEGISandData/COVID-19/master/")
}

GovexMaster <- function() {
  return("govex/COVID-19/master/")
}

GxTablesUSData <- function() {
  return("data_tables/vaccine_data/us_data/")
}

JHU_repository <- function() {
  paste(GithubUserContent(),
        CSSEGICOVID19Master(),
        "csse_covid_19_data/",
        sep = "")
}

filenameOfStateLevelDataForDate <- function(aDate) {
  paste(jhuFileDateString(aDate), ".csv", sep="")
}

pathnameOfStateLevelUpdateDataForDate <- function(aDate) {
  paste("./DATA/",filenameOfStateLevelDataForDate(aDate), sep = "")
}

updateStateLevelDataForDate_URL <- function(aDate) {
  paste(JHU_repository(),
        "csse_covid_19_daily_reports_us/",
        filenameOfStateLevelDataForDate(aDate),
        sep = "")
}

updateData_URL <- function() {
  updateStateLevelDataForDate_URL(expectedLatestUpdateDataDate())
}

repositoryPathForDate <- function(aDate) {
  paste(JHU_repository(),
        "csse_covid_19_daily_reports/",
        filenameOfStateLevelDataForDate(aDate),
        sep = "")
}

# Synthesize the URL of the Johns Hopkins time series data file
# Arguments: type = {"confirmed", "deaths"}, locale = {"US", "global"}
TS_URL <- function(type, locale) {
  U_out <- paste(JHU_repository(),
                 "csse_covid_19_time_series/",
                 "time_series_covid19_",
                 type, "_", locale, ".csv",
                 sep = "")
}

# Good for daily update, not so much if you lost a day
#   https://raw.githubusercontent.com/"
#   "govex/COVID-19/master/"
#   "data_tables/vaccine_data/us_data/hourly/vaccine_data_us.csv
Vacc_URL <- function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 GxTablesUSData(),
                 "hourly/vaccine_data_us.csv",
                 sep = "")
}

# Whole time series, megabytes to crunch but good for major reconstruction
Vacc_TS_URL <- function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 GxTablesUSData(),
                 "/time_series/vaccine_data_us_timeline.csv",
                 sep = "")
}

# saveJHUHereForDate <- function(aDate) {
#   paste("F:/DeanDocuments/COVID_DATA/JHU/",
#         jhuFileDateString(aDate), ".csv",
#         sep = "")
# }

downloadAndSaveStateLevelUpdateData <- function(aDate,
                                                traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered downloadAndSaveStateLevelUpdateData\n")
  }

  if (url.exists(updateStateLevelDataForDate_URL(aDate))) {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "Remote file", JHU_repository(), "\n")
      cat(file = stderr(), myPrepend, "           ",
          paste("csse_covid_19_daily_reports_us/",
                filenameOfStateLevelDataForDate(aDate),
                "\n", sep = "")) 
      cat(file = stderr(), myPrepend, "     exists, will try to download\n")
    }
    # Get the first file
    updateTibble <- try(read_csv(updateStateLevelDataForDate_URL(aDate),
                                 col_types = cols(.default = col_double(),
                                                  Province_State = col_character(),
                                                  Country_Region = col_character(),
                                                  Last_Update = col_datetime(format = ""),
                                                  People_Hospitalized = col_logical(),
                                                  ISO3 = col_character(),
                                                  Hospitalization_Rate = col_logical())))
    if (class(updateTibble)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend,
          "download of", updateStateLevelDataForDate_URL(aDate), "failed\n")
      }
      cat(file = stderr(), myPrepend, "FATAL ERROR -- UNABLE TO DOWNLOAD UPDATE DATA\n")
      stop()
    }

    updateTibble <- updateTibble %>%
      filter(!str_detect(Province_State, "Princess"))
      
    write_csv(updateTibble, pathnameOfStateLevelUpdateDataForDate(aDate))

    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend,
        "Downloaded and wrote", pathnameOfStateLevelUpdateDataForDate(aDate), "\n")
    }
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend,
        "url.exists returns FALSE:", updateStateLevelDataForDate_URL(aDate), "\n")
    }
    cat(file = stderr(), myPrepend, "FATAL ERROR -- NO UPDATE DATA AVAILABLE\n")
    stop()
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving downloadAndSaveStateLevelUpdateData\n")
  }
}

processFileForDate <- function(aDate, traceThisRoutine = FALSE, prepend = "") {
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
 
# firstDate <- as.Date("2021-04-01")
# processFileForDate(firstDate)

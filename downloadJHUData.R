# https://github.com/CSSEGISandData/COVID-19/blob/master/
#   csse_covid_19_data/csse_covid_19_daily_reports/
#   07-15-2021.csv

# "https://raw.githubusercontent.com/"
# "CSSEGISandData/COVID-19/master/"
# "csse_covid_19_data/"

#*************************************************************
#*
#* Data for first date for major reconstruction
#* repositoryPathForDate(<2021-04-01 as date>)
#* 
#*************************************************************
# https://raw.githubusercontent.com/
# CSSEGISandData/COVID-19/master/
# csse_covid_19_data/
# csse_covid_19_daily_reports/04-01-2021.csv

#*************************************************************
#*
#* Most recent vaccination data (typically updated earlier today)
#* Vacc_URL()
#* 
#*************************************************************
# https://raw.githubusercontent.com/"
# "govex/COVID-19/master/"
# "data_tables/vaccine_data/us_data/hourly/vaccine_data_us.csv

#*************************************************************
#*
#* Where to find state data, and when each one updates
#* 
#************************************************************* 
# https://raw.githubusercontent.com/
# govex/COVID-19/master/
# data_tables/US_reporting_frequency.csv

#*************************************************************
#*
#* Fields of vaccine_data archive file
#* 
#************************************************************* 
# https://raw.githubusercontent.com/
# govex/COVID-19/master/
# data_tables/vaccine_data/archive/data_dictionary.csv

#*************************************************************
#*
#* US vaccine time series data. Vacc_TS_URL()
#* It's huge but has all data from 2020-12-10 thru present
#*  (checked 2021-08-19; was )
#* 
#************************************************************* 
# https://raw.githubusercontent.com/
# govex/COVID-19/master/
# data_tables/vaccine_data/us_data/time_series/vaccine_data_us_timeline.csv

#************************************************************* 
#*
#* US People vacinated time series data. PVacc_TS_URL()
# https://raw.githubusercontent.com/
# govex/COVID-19/master/
# data_tables/vaccine_data/us_data/time_series/people_vaccinated_us_timeline.csv

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

Vacc_Cols <- function() {
  C_out <- cols(.default = col_double(),
                Province_State = col_character(),
                Country_Region = col_character(),
                Date = col_date(format = ""),
                Vaccine_Type = col_character(),
                Combined_Key = col_character())
}

# Whole time series, megabytes to crunch but good for major reconstruction
Vacc_TS_URL <- function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 GxTablesUSData(),
                 "/time_series/vaccine_data_us_timeline.csv",
                 sep = "")
}

Vacc_TS_Cols <- function() {
  C_out <- cols(.default = col_double(),
                Province_State = col_character(),
                Country_Region = col_character(),
                Date = col_date(format = ""),
                Vaccine_Type = col_character(),
                Combined_Key = col_character())
}

Vacc_TS_Path <- function() {
  P_out <- paste("DATA/VaccUpdate_",
                 jhuFileDateString(Sys.Date()),
                 sep = "")
}

#* US People vacinated time series data. Big but not so big as above
PVacc_TS_URL <- function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 GxTablesUSData(),
                 "/time_series/people_vaccinated_data_us_timeline.csv",
                 sep = "")
}

getURLOrStop <- function(aURL, col_types, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered getURLOrStop\n")
  }
  if (url.exists(aURL)) {
    rawData <- try(read_csv(aURL,
                            col_types = col_types))
    if (class(rawData)[1] == "try-error") {
      if (traceThisRoutine) {
        cat(file = stderr(), "try(read_csv()) failed for ", aURL, "\n")
      }
      stop(paste("FATAL ERROR -- Unable to download:", aURL))
    } 
  } else {
    if (traceThisRoutine) {
      cat(file = stderr(), "url.exists returned FALSE for ", aURL, "\n")
    }
    stop(paste("FATAL ERROR -- No such URL: ", aURL))
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving getURLOrStop\n")
  }
  return(rawData)
}

getURLFromSpecsOrStop <- function(theSpecs, traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered getURLFromSpecsOrStop\n")
  }
  
  theData <- getURLOrStop(theSpecs$URL, theSpecs$COLS,
                          traceThisRoutine = traceThisRoutine,
                          prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving getURLFromSpecsOrStop\n")
  }
  
  return(theData)
}

vaccDailyUpdateDataSpecs <- function() {
  list(URL = Vacc_URL(),
       COLS = Vacc_Cols(),
       PATH = paste("DATA/VaccUpdate_",
                    jhuFileDateString(Sys.Date()),
                    sep = ""))
}

vaccTimeSeriesDataSpecs <- function() {
  list(URL = Vacc_TS_URL(),
       COLS = Vacc_TS_Cols(),
       PATH = paste("DATA/VaccTS_",
                    jhuFileDateString(Sys.Date()),
                    sep = ""))
}

#* US People vacinated time series data.
pVaccTimeSeriesDataSpecs <- function() {
  list(URL = PVacc_TS_URL(),
       COLS = Vacc_TS_Cols(),
       PATH = paste("DATA/pVaccUpdate_",
                    jhuFileDateString(Sys.Date()),
                    sep = ""))
}

downloadVaccDailyUpdateData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered downloadVaccDailyUpdateData\n")
  }

  rawData <- getURLFromSpecsOrStop(vaccDailyUpdateDataSpecs(),
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)

  specs <- vaccDailyUpdateDataSpecs()
  rawData1 <- getURLOrStop(specs$URL, col_types = specs$COLS,
                          traceThisRoutine = traceThisRoutine,
                          prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving downloadVaccDailyUpdateData\n")
  }

  return(list(RD = rawData, RD1 = rawData1))
}

downloadAndSaveVaccDailyUpdateData <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered downloadAndSaveVaccDailyUpdateData\n")
  }

  specs <- vaccDailyUpdateDataSpecs()
  rawData <- getURLOrStop(specs$URL, col_types = specs$COLS,
                          traceThisRoutine = traceThisRoutine,
                          prepend = myPrepend)
  
  write_csv(updateTibble, specs$PATH)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving downloadAndSaveVaccDailyUpdateData\n")
  }
  return(rawData)
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
  
  updateTibble <- getURLOrStop(updateStateLevelDataForDate_URL(aDate),
                               col_types = cols(.default = col_double(),
                                                Province_State = col_character(),
                                                Country_Region = col_character(),
                                                Last_Update = col_datetime(format = ""),
                                                People_Hospitalized = col_logical(),
                                                ISO3 = col_character(),
                                                Hospitalization_Rate = col_logical()))

  updateTibble <- updateTibble %>%
    filter(!str_detect(Province_State, "Princess"))
      
  write_csv(updateTibble, pathnameOfStateLevelUpdateDataForDate(aDate))

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend,
        "Downloaded and wrote", pathnameOfStateLevelUpdateDataForDate(aDate), "\n")
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

source("pathnameFunctions.R")
source("dateFormatRoutines.R")
source("mostRecentDataDate.R")

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

csseData <- function() {
  return("csse_covid_19_data/")
}

csseDaily <- function() {
  return("csse_covid_19_daily_reports/")
}

csseDailyUS <- function() {
  return("csse_covid_19_daily_reports_us/")
}

csseTS <- function() {
  return("csse_covid_19_time_series/")
}

dataTablesVaccData <- function() {
  return("data_tables/vaccine_data/")
}

USDataTS <- function() {
  return("us_data/time_series/")
}

USDataHourly <- function() {
  return("us_data/hourly/")
}

JHU_repository <- function() {
  paste(GithubUserContent(),
        CSSEGICOVID19Master(),
        csseData(),
        sep = "")
}

updateSansCountyDataForDate_URL <- function(aDate) {
  paste(JHU_repository(),
        csseDailyUS(),
        filenameOfSansCountyDataForDate(aDate),
        sep = "")
}

updateStateLevelDataForDate_URL <- function(aDate) {
  paste(JHU_repository(),
        csseDailyUS(),
        filenameOfStateLevelDataForDate(aDate),
        sep = "")
}

updateData_URL <- function() {
  updateStateLevelDataForDate_URL(expectedLatestUpdateDataDate())
}

JHUDailyStateDataURLForDate <- function(aDate) {
  U_out <- paste(JHU_repository(),
                 csseDaily(),
                 filenameOfStateLevelDataForDate(aDate),
                 sep = "")
}

# Synthesize the URL of the Johns Hopkins time series data file
# Arguments: type = {"confirmed", "deaths"}, locale = {"US", "global"}
TS_URL <- function(type, locale) {
  U_out <- paste(JHU_repository(),
                 csseTS(),
                 "time_series_covid19_",
                 type, "_", locale, ".csv",
                 sep = "")
}

# Most recent vaccination data (typically updated earlier today)
# Good for daily update, not so much if you lost a day
#   https://raw.githubusercontent.com/"
#   "govex/COVID-19/master/"
#   "data_tables/vaccine_data/us_data/hourly/vaccine_data_us.csv
Vacc_URL <- function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 dataTablesVaccData(),
                 USDataHourly(),
                 "vaccine_data_us.csv",
                 sep = "")
}

# VaccTimeline_URL <- function() {
#   U_out <- paste(GithubUserContent(),
#                  GovexMaster(),
#                  dataTablesVaccData(),
#                  USDataTS(),
#                  "vaccine_data_us_timeline.csv",
#                  sep = "")
# }

#*************************************************************
#*
#* US vaccine time series data. Vacc_TS_URL()
#* Megabytes to crunch; has all data from 2020-12-10 thru present,
#* good for major reconstruction
Vacc_TS_URL <- function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 dataTablesVaccData(),
                 USDataTS(),
                 "vaccine_data_us_timeline.csv",
                 sep = "")
}

#* US People vacinated time series data. Big but not so big as above
PVacc_TS_URL <- function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 dataTablesVaccData(),
                 USDataTS(),
                 "people_vaccinated_data_us_timeline.csv",
                 sep = "")
}

peopleVacc_URL <-  function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 dataTablesVaccData(),
                 USDataTS(),
                 "people_vaccinated_us_timeline.csv",
                 sep = "")
  
}

# START -- reference unmodified calls
JHU_repository_OLD <- function() {
  paste(GithubUserContent(),
        CSSEGICOVID19Master(),
        "csse_covid_19_data/",
        sep = "")
}

updateSansCountyDataForDate_URL_OLD <- function(aDate) {
  paste(JHU_repository(),
        "csse_covid_19_daily_reports_us/",
        filenameOfSansCountyDataForDate(aDate),
        sep = "")
}

updateStateLevelDataForDate_URL_OLD <- function(aDate) {
  paste(JHU_repository(),
        "csse_covid_19_daily_reports_us/",
        filenameOfStateLevelDataForDate(aDate),
        sep = "")
}

JHUDailyStateDataURLForDate_OLD <- function(aDate) {
  U_out <- paste(JHU_repository(),
                 "csse_covid_19_daily_reports/",
                 filenameOfStateLevelDataForDate(aDate),
                 sep = "")
}

TS_URL_OLD <- function(type, locale) {
  U_out <- paste(JHU_repository(),
                 "csse_covid_19_time_series/",
                 "time_series_covid19_",
                 type, "_", locale, ".csv",
                 sep = "")
}

Vacc_URL_OLD <- function() {
  U_out <- paste("https://raw.githubusercontent.com/",
                 "govex/COVID-19/master/",
                 "data_tables/vaccine_data/",
                 "us_data/hourly/vaccine_data_us.csv",
                 sep = "")
}

# VaccTimeline_URL_OLD <- function() {
#   U_out <- paste("https://raw.githubusercontent.com/",
#                  "govex/COVID-19/master/",
#                  "data_tables/vaccine_data/",
#                  "us_data/time_series/",
#                  "vaccine_data_us_timeline.csv",
#                  sep = "")
# }

#*************************************************************
#*
#* US vaccine time series data. Vacc_TS_URL()
#* Megabytes to crunch; has all data from 2020-12-10 thru present,
#* good for major reconstruction
Vacc_TS_URL_OLD <- function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 GxTablesUSData(),
                 "time_series/vaccine_data_us_timeline.csv",
                 sep = "")
}

peopleVacc_URL_OLD <-  function() {
  U_out <- paste("https://raw.githubusercontent.com/",
                 "govex/COVID-19/master/",
                 "data_tables/vaccine_data/",
                 "us_data/time_series/",
                 "people_vaccinated_us_timeline.csv",
                 sep = "")
  
}

#* US People vacinated time series data. Big but not so big as above
PVacc_TS_URL_OLD <- function() {
  U_out <- paste(GithubUserContent(),
                 GovexMaster(),
                 GxTablesUSData(),
                 "time_series/people_vaccinated_data_us_timeline.csv",
                 sep = "")
}

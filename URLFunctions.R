source("pathnameFunctions.R")
source("dateFormatRoutines.R")
source("mostRecentDataDate.R")

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

updateData_URL <- function() {
  paste(JHU_repository(),
        "csse_covid_19_daily_reports_us/",
        jhuFileDateString(expectedLatestUpdateDataDate()),
        ".csv",
        sep = "")
}

updateSansCountyDataForDate_URL <- function(aDate) {
  paste(JHU_repository(),
        "csse_covid_19_daily_reports_us/",
        filenameOfSansCountyDataForDate(aDate),
        sep = "")
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

JHUDailyStateDataURLForDate <- function(aDate) {
  U_out <- paste(JHU_repository(),
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
  U_out <- paste("https://raw.githubusercontent.com/govex/COVID-19/",
                 "master/data_tables/vaccine_data/",
                 "us_data/hourly/vaccine_data_us.csv",
                 sep = "")
}

VaccTimeline_URL <- function() {
  U_out <- paste("https://raw.githubusercontent.com/govex/COVID-19/",
                 "master/data_tables/vaccine_data/",
                 "us_data/time_series/",
                 "vaccine_data_us_timeline.csv",
                 sep = "")
}

peopleVacc_URL <-  function() {
  U_out <- paste("https://raw.githubusercontent.com/govex/COVID-19/",
                 "master/data_tables/vaccine_data/",
                 "us_data/time_series/",
                 "people_vaccinated_us_timeline.csv",
                 sep = "")
  
}

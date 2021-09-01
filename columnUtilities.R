library(tidyverse)

# For Johns Hopkins time series data files
TSColTypes <- function() {
  return(cols(.default = col_double(),
              iso2 = col_character(),
              iso3 = col_character(),
              Admin2 = col_character(),
              Province_State = col_character(),
              Country_Region = col_character(),
              Combined_Key = col_character()))
}

# for US_Vaccinations.csv and US_State_Vaccinations.csv
vaccColTypes <- function() {
  return(cols(.default = col_double(),
              Combined_Key = col_character(),
              Datum = col_character(),
              Loc_Datum = col_character()))
}

# Most of the files I wrote from JHU data
myTSColTypes <- function() {
  return(cols(.default = col_double(),
              Province_State = col_character(),
              Combined_Key = col_character()))
}

# Numeric and Combined_Key
justCKTypes <- function() {
  return(cols(.default = col_double(),
              Combined_Key = col_character()))
}


# JHU daily reports
dataFileColTypes <- function() {
  return(cols(.default = col_double(),
                         Province_State = col_character(),
                         Country_Region = col_character(),
                         Last_Update = col_datetime(format = ""),
                         Recovered = col_logical(),
                         Active = col_logical(),
                         People_Hospitalized = col_logical(),
                         ISO3 = col_character()))
}

# US_Population.csv
populationColTypes <- function() {
  return(cols(.default = col_character(),
              FIPS = col_integer(),
              Population = col_integer()))
}

nColumnsBeforeDates <- function(aTibble) {
  theNames <- names(aTibble)
  nCols <- length(theNames)
  warnOption = getOption("warn")
  options(warn = -1)
  nBeforeDates <- 0
  while((nBeforeDates < nCols) && is.na(mdy(theNames[nBeforeDates + 1]))) {
    nBeforeDates <- nBeforeDates + 1
  }
  options(warn = warnOption)
  nBeforeDates
}

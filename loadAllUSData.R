library(tidyverse)
library(lubridate)

source("./computeNewAndGrowth.R")
source("./loadUSConfirmedData.R")
source("./loadUSDeathsData.R")
source("./loadUSPeopleTestedData.R")
source("./loadUSHospitalizationRateData.R")
source("./loadUSIncidentRateData.R")
source("./loadUSMortalityRateData.R")
source("./loadUSPeopleHospitalizedData.R")
source("./loadUSTestingRateData.R")
source("./loadUSVaccinationData.R")

loadAllUSData <- function() {
  aDate = today("EST")

  US_Population <<- read_csv("./DATA/US_Population.csv",
                            col_types=cols(FIPS = col_double(),
                                           Province_State = col_character(),
                                           Combined_Key = col_character(),
                                           CountyName = col_character(),
                                           Population = col_double()))
  
  loadUSVaccinationData()

  loadUSConfirmedData()
  
  loadUSDeathsData()
  
  loadUSPeopleTestedData()
  
  loadUSHospitalizationRateData()
  
  loadUSIncidentRateData()

  loadUSMortalityRateData(aDate)

  loadUSPeopleHospitalizedData()

  loadUSTestingRateData()

  CountiesByState <<- US_County_Confirmed %>%
    mutate(State = Province_State, County = Admin2, .keep="none") %>%
    filter(str_detect(County, "Out of", negate=TRUE)) %>%
    filter(str_detect(County, "Unassigned", negate=TRUE)) %>%
    filter(str_detect(County, "Dukes and Nantucket", negate=TRUE)) %>%
    filter(str_detect(County, "Michigan Department of Corrections", negate=TRUE)) %>%
    filter(str_detect(County, "Federal Correctional Institution", negate=TRUE)) %>%
    filter(str_detect(County, "Central Utah", negate=TRUE)) %>%
    filter(str_detect(County, "Southeast Utah", negate=TRUE)) %>%
    filter(str_detect(County, "Southwest Utah", negate=TRUE)) %>%
    filter(str_detect(County, "TriCounty", negate=TRUE)) %>%
    filter(str_detect(County, "Weber-Morgan", negate=TRUE))

  States <<- unique(CountiesByState$State)
}

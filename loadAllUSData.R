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

loadATypeOfData <- function(US_leaf, State_leaf, County_leaf, theType) {
  readLeaf <- function(aLeaf) {
    aPath <- paste("./DATA/", aLeaf, ".csv", sep = "")
    aTibble <- read_csv(aPath,
                        col_types=cols(.default = col_double(),
                                       Province_State = col_character(),
                                       Combined_Key = col_character()))
  }
  newFromCumulative <- function(inputTibble, updateToThisDate,
                                theScope, theType,
                                nDays = 35, nFirst = 3) {
    outputList <- computeNewOnDayAndGrowthRate(inputTibble,
                                               updateToThisDate,
                                               nDays, nFirst,
                                               getGrowthRate = FALSE,
                                               nonzeroOnly = FALSE,
                                               tibbleName = paste(theScope,
                                                                  theType,
                                                                  "Cumulative",
                                                                  sep = ""))
    outputTibble <- outputList$new
  }
  movingAverageFromCumulative <- function(inputTibble, updateToThisDate,
                                          theScope, theType,
                                          nDays = 28, nAvg = 7, nFirst = 3) {
    dims <- dim(inputTibble)
    startDataCol <- dims[2] - nDays - nAvg
    endDataCol <- dims[2]
    HiOut <- inputTibble %>%
      select(1:{nFirst}, {startDataCol}:{endDataCol})
    LoOut <- inputTibble %>%
      select(1:{nFirst}, {startDataCol - 1}:{endDataCol - 1})
    
    movingAverageGrowth(inputTibble, updateToThisDate, nDays, nAvg, nFirstCols = nFirst,
                        tibbleName = paste(theScope, theType, "Cumulative", sep = ""))
  }
  
  updateToThisDate <- expectedLatestUpdateDataDate()
  updateTimeSeriesDataFilesAsNecessary()
  
  US_Cumulative <- readLeaf(US_leaf)
  US_New        <- newFromCumulative(US_Cumulative, updateToThisDate, "US", theType)
  
  State_Cumulative <- readLeaf(State_leaf)
  State_New        <- newFromCumulative(State_Cumulative, updateToThisDate,
                                        "State", theType)
  
  US_Cumulative_A7 <- movingAverageFromCumulative(US_Cumulative, updateToThisDate,
                                                  theScope, theType)
  # US_New_A7        <- movingAverageFromCumulative(US_New, updateToThisDate,
  #                                                 theScope, theType)
  State_Cumulative_A7 <- movingAverageFromCumulative(State_Cumulative, updateToThisDate,
                                                     theScope, theType)
  # State_New_A7        <- movingAverageFromCumulative(State_New, updateToThisDate,
  #                                                    theScope, theType)
  
  list(US_C = US_Cumulative, US_N = US_New,
       US_C_A = US_Cumulative_A7, # US_N_A = US_New_A7,
       State_C = State_Cumulative, State_N = State_New,
       State_C_A = State_Cumulative_A7) #, State_N_A = State_New_A7)
}

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

  CountiesByState <<- US_County_Confirmed_Cumulative %>%
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

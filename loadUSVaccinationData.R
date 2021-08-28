source("./mostRecentDataDate.R")
source("./updateStateLevelSerializedDataFiles.R")
source("./computeNewAndGrowth.R")

# loadUSVaccinationData <- function() {
#   rem1000 <- function(n) {
#     as.integer(n - 1000 * floor(n / 1000))
#   }
# 
#   updateToThisDate <- today("EST")
# 
#   US_Vaccinations_As_Filed <- read_csv("./DATA/US_Vaccinations.csv",
#                                 col_types=cols(.default = col_double(),
#                                                Combined_Key = col_character(),
#                                                Datum = col_character(),
#                                                Loc_Datum = col_character()))
# 
#   US_State_Vaccinations_As_Filed <- read_csv("./DATA/US_State_Vaccinations.csv",
#                                      col_types=cols(.default = col_double(),
#                                                     Combined_Key = col_character(),
#                                                     Datum = col_character(),
#                                                     Loc_Datum = col_character()))
# 
#   US_Vaccinations_A7 <<- movingAverageData(US_Vaccinations,
#                                            updateToThisDate,
#                                            28, 7,
#                                            tibbleName="US_Vaccinations")
#   US_State_Vaccinations_A7 <<- movingAverageData(US_State_Vaccinations,
#                                                  updateToThisDate,
#                                                  28, 7,
#                                                  tibbleName="US_State_Vaccinations")
#   
#   US_Vaccination_Pcts_A7 <<- US_Vaccinations_A7
#   US_State_Vaccination_Pcts_A7 <<- US_State_Vaccinations_A7
# }

loadUSVaccinationData <- function(traceThisRoutine = FALSE, prepend = "") {
  # Local function
  rem1000 <- function(n) {
    as.integer(n - 1000 * floor(n / 1000))
  }

  # Mainline of this routine
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSVaccinationData\n")
  }

  updateToThisDate <- today("EST")
  
  US_Vaccinations_As_Filed <- read_csv("./DATA/US_Vaccinations.csv",
                                       col_types=cols(.default = col_double(),
                                                      Combined_Key = col_character(),
                                                      Datum = col_character(),
                                                      Loc_Datum = col_character()))
  
  US_State_Vaccinations_As_Filed <- read_csv("./DATA/US_State_Vaccinations.csv",
                                             col_types=cols(.default = col_double(),
                                                            Combined_Key = col_character(),
                                                            Datum = col_character(),
                                                            Loc_Datum = col_character()))
  
  pop_Data <- read_csv("./DATA/US_Population.csv",
                       col_types=cols(FIPS = col_double(),
                                      Province_State = col_character(),
                                      Combined_Key = col_character(),
                                      CountyName = col_character(),
                                      Population = col_double())) %>%
    mutate(FIPSREM = rem1000(FIPS), .keep = "all") %>% # New column is remainder of FIPS / 1000
    filter(FIPSREM == 0) %>%                           # This selects US & States
    select(-FIPSREM) %>%                               # Discard that column
    select(Province_State, Population) %>%
    mutate(Doses_alloc = 0, Doses_shipped = 0, Doses_admin = 0,
           Stage_One_Doses = 0, Stage_Two_Doses = 0, .keep ="all") %>%
    gather(key="Datum", "Zero", 3:7) %>%
    mutate(Loc_Datum = paste(Province_State, Datum, sep="_"), .keep = "all")
  
  US_PopData <- semi_join(pop_Data, US_Vaccinations_As_Filed, by = "Loc_Datum") %>%
    select(Loc_Datum, Population)
  US_State_PopData <- semi_join(pop_Data, US_State_Vaccinations_As_Filed, by = "Loc_Datum") %>%
    select(Loc_Datum, Population)
  
  US_Vaccination_WithPop <- left_join(US_PopData, 
                                   US_Vaccinations_As_Filed,
                                   by = "Loc_Datum") %>%
    select(-Loc_Datum) %>%
    mutate(Combined_Key = Combined_Key, Datum = Datum, Population = Population, .keep="unused")

  US_State_Vaccination_WithPop <- left_join(US_State_PopData,
                                         US_State_Vaccinations_As_Filed,
                                         by = "Loc_Datum") %>%
    select(-Loc_Datum) %>%
    mutate(Combined_Key = Combined_Key, Datum = Datum, Population = Population, .keep="unused")
  
  usDims = dim(US_Vaccination_WithPop)
  usStDm = dim(US_State_Vaccination_WithPop)
  
  US_Vaccination_Pcts <<- US_Vaccination_WithPop %>%
    mutate(across(4:usDims[2], ~ 100.0 * .x / Population))
  US_State_Vaccination_Pcts <<- US_State_Vaccination_WithPop %>%
    mutate(across(4:usDims[2], ~ 100.0 * .x / Population))
  
  getNAvgs <- min(28, (updateToThisDate - as.Date('2021-03-23'))) 
  US_Vaccination_Pcts_A7 <<- movingAverageData(US_Vaccination_Pcts,
                                               updateToThisDate,
                                               getNAvgs, 7,
                                               tibbleName="US_Vaccination_Pcts",
                                               traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  US_State_Vaccination_Pcts_A7 <<- movingAverageData(US_State_Vaccination_Pcts,
                                                     updateToThisDate,
                                                     getNAvgs, 7,
                                                     tibbleName="US_State_Vaccination_Pcts",
                                                     traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSVaccinationData\n")
  }
}

source("mostRecentDataDate.R")
source("updateStateLevelSerializedDataFiles.R")
source("computeNewAndGrowth.R")

# loadUSVaccinationData <- function() {
#   rem1000 <- function(n) {
#     as.integer(n - 1000 * floor(n / 1000))
#   }
# 
#   updateToThisDate <- today("EST")
# 
#   US_Vaccinations_As_Filed <- read_csv("./DATA/US_Vaccinations.csv",
#                                 col_types = vaccColTypes())
# 
#   US_State_Vaccinations_As_Filed <- read_csv("./DATA/US_State_Vaccinations.csv",
#                                      col_types = vaccColTypes())
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
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSVaccinationData\n")
  }

  computeCounty <- FALSE
  computeNew <- FALSE
  computeAvg <- TRUE
  computePercent <- TRUE

  results <- loadATypeOfData("Vaccinations", vaccColTypes(), computeCounty,
                              computeNew, computeAvg, computePercent,
                              traceThisRoutine = FALSE, prepend = myPrepend)
  
  US_Vaccination_Pcts <<- results$US_C_P
  US_State_Vaccination_Pcts <<- results$State_C_P
  US_Vaccination_Pcts_A7 <<- results$US_C_PA7
  US_State_Vaccination_Pcts_A7 <<- results$State_C_PA7
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSVaccinationData\n")
  }
}
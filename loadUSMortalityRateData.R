source("updateStateLevelSerializedDataFiles.R")
source("columnUtilities.R")

loadUSMortalityRateData <- function(aDate) {
  updateStateLevelSerializedDataFilesAsNecessary()
  
  US_Mortality_Rate <<- read_csv("./DATA/US_Case_Fatality_Ratio.csv",
                                 col_types = myTSColTypes())
  US_State_Mortality_Rate <<- read_csv("./DATA/US_State_Case_Fatality_Ratio.csv",
                                       col_types = justCKColTypes())
}

source("./updateStateLevelSerializedDataFiles.R")

loadUSMortalityRateData <- function(aDate) {
  updateStateLevelSerializedDataFilesAsNecessary()
  
  US_Mortality_Rate <<- read_csv("./DATA/US_Case_Fatality_Ratio.csv",
                                 col_types = cols(.default = col_double(),
                                                  Province_State = col_logical(),
                                                  Combined_Key = col_character()))
  US_State_Mortality_Rate <<- read_csv("./DATA/US_State_Case_Fatality_Ratio.csv",
                                       col_types = cols(.default = col_double(),
                                                        Combined_Key = col_character()))
}

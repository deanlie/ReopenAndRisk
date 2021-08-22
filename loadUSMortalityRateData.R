source("./updateStateLevelSerializedDataFiles.R")
source("./variableFieldNames.R")

loadUSMortalityRateData <- function(aDate) {
  vfn <- variableFieldNames(aDate)

  updateStateLevelSerializedDataFilesAsNecessary()
  
  US_Mortality_Rate <<- read_csv(paste("./DATA/US_", vfn$mortality, ".csv", sep = ""),
                                 col_types = cols(.default = col_double(),
                                                  Province_State = col_logical(),
                                                  Combined_Key = col_character()))
  US_State_Mortality_Rate <<- read_csv(paste("./DATA/US_State_", vfn$mortality, ".csv", sep = ""),
                                       col_types = cols(.default = col_double(),
                                                        Combined_Key = col_character()))
}

source("./updateStateLevelSerializedDataFiles.R")

loadUSTestingRateData <- function() {
  updateStateLevelSerializedDataFilesAsNecessary()
  
  US_Testing_Rate <<- read_csv("./DATA/US_Testing_Rate.csv",
                               col_types = cols(.default = col_double(),
                                                Province_State = col_logical(),
                                                Combined_Key = col_character()))
  US_State_Testing_Rate <<- read_csv("./DATA/US_State_Testing_Rate.csv",
                                     col_types = cols(.default = col_double(),
                                                      Combined_Key = col_character()))
}
  
library(tidyverse)

add_Prov_State_to_TotalTestResults <- function() {
  munge <- function(aString) {
    str_replace(aString, ", US", "")
  }
  orig_file <- read_csv("./Data/US_State_Total_Test_Results_0.csv",
                        col_types = cols(.default = col_double(),
                                         Combined_Key = col_character()))
  new_file <- mutate(orig_file, Province_State = munge(Combined_Key), .keep = "all", .before = Combined_Key)
  write_csv(new_file, "./DATA/US_State_Total_Test_Results.csv")
}

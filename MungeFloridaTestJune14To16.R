library(tidyverse)

dataFileColSpec <- function() {
  cols(.default = col_double(),
       Province_State = col_character(),
       Country_Region = col_character(),
       Last_Update = col_datetime(format = ""),
       Recovered = col_logical(),           # all NAs so "double" doesn't work
       Active = col_logical(),              # ditto
       People_Hospitalized = col_logical(), # ditto
       ISO3 = col_character())
}

D613 <- read_csv("./DATA/06-13-2021.csv",
                 col_types = dataFileColSpec())

for (dayPart in c("14", "15", "16")) {
  thePath <- paste("./DATA/06-", dayPart, "-2021.csv", sep = "")
  mungeMe <- read_csv(thePath, col_types = dataFileColSpec())
  mungeMe[12, 12] <- D613[12, 12]
  mungeMe[12, 17] <- D613[12, 17]
  write_csv(mungeMe, thePath)
}

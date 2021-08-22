library(lubridate)

variableFieldNames <- function(aDate) {
  nameChangeDate1 <- as_date("2020-11-09")

  numberTested <- "Total_Test_Results"
  mortality <- "Case_Fatality_Ratio"

    if (aDate < nameChangeDate1) {
    numberTested <- "People_Tested"
    mortality <- "Mortality_Rate"
  }

  list(numberTested=numberTested, mortality=mortality, nameChangeDate1=nameChangeDate1)
}
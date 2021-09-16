library(lubridate)

variableFieldNames <- function(aDate) {
  list(numberTested="Total_Test_Results",
       mortality="Case_Fatality_Ratio",
       nameChangeDate1=as_date("2020-11-09"))
}
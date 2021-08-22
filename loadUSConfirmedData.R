source("./mostRecentDataDate.R")
source("./updateTimeSeriesDataFilesAsNecessary.R")
source("./computeNewAndGrowth.R")

loadUSConfirmedData <- function() {
  updateToThisDate <- expectedLatestUpdateDataDate()
  updateTimeSeriesDataFilesAsNecessary()

  AllConfirmedData <- loadATypeOfData("US_Deaths", "US_State_Deaths", "US_County_Deaths", "Deaths")
  
  US_Confirmed_Cumulative <<- AllConfirmedData$US_C %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_State_Confirmed_Cumulative <<- AllConfirmedData$State_C %>%
    filter(!str_detect(Combined_Key, "Princess"))
  
  US_Confirmed_Cumulative_A7 <- AllConfirmedData$US_C_A %>%
    filter(!str_detect(Combined_Key, "Princess"))
  US_State_Confirmed_Cumulative_A7 <- AllConfirmedData$State_C_A %>%
    filter(!str_detect(Combined_Key, "Princess"))
  
  # US_Confirmed_Cumulative <<- read_csv("./DATA/US_Confirmed.csv",
  #                                      col_types=cols(.default = col_double(),
  #                                                     Province_State = col_logical(),
  #                                                     Combined_Key = col_character())) %>%
  #   filter(!str_detect(Combined_Key, "Princess"))
  # US_State_Confirmed_Cumulative <<- read_csv("./DATA/US_State_Confirmed.csv",
  #                                            col_types=cols(.default = col_double(),
  #                                                           Province_State = col_character(),
  #                                                           Combined_Key = col_character())) %>%
  #   filter(!str_detect(Combined_Key, "Princess"))
  US_County_Confirmed_Cumulative <<- read_csv("./DATA/US_County_Confirmed.csv",
                                              col_types=cols(.default = col_double(),
                                                             Admin2 = col_character(),
                                                             Province_State = col_character(),
                                                             Combined_Key = col_character())) %>%
    filter(!str_detect(Combined_Key, "Princess"))

  # US_Confirmed_New <<- computeNewOnDayAndGrowthRate(US_Confirmed_Cumulative,
  #                                                   updateToThisDate,
  #                                                   nDays = 28, nFirst = 3,
  #                                                   getGrowthRate = FALSE,
  #                                                   nonzeroOnly = FALSE,
  #                                                   tibbleName = "US_Confirmed_Cumulative")
  # US_Confirmed_New_A7 <<- movingAverageGrowth(US_Confirmed_Cumulative,
  #                                             updateToThisDate,
  #                                             28, 7, nFirstCols=3,
  #                                             tibbleName="US_Confirmed_Cumulative")
  # US_State_Confirmed_New <<- computeNewOnDayAndGrowthRate(US_State_Confirmed_Cumulative,
  #                                                      updateToThisDate,
  #                                                      nDays = 28, nFirst = 3,
  #                                                      getGrowthRate = FALSE,
  #                                                      nonzeroOnly = FALSE,
  #                                                      tibbleName = "US_State_Confirmed_Cumulative")
  # US_State_Confirmed_New_A7 <<- movingAverageGrowth(US_State_Confirmed_Cumulative,
  #                                               updateToThisDate,
  #                                               28, 7, nFirstCols=3,
  #                                               tibbleName="US_State_Confirmed_Cumulative")
  # US_County_Confirmed_New_A7 <<- movingAverageGrowth(US_County_Confirmed_Cumulative,
  #                                                updateToThisDate,
  #                                                28, 7, nFirstCols=3,
  #                                                tibbleName="US_County_Confirmed_Cumulative")

  # US_Confirmed_A7 <<- movingAverageData(US_Confirmed_Cumulative,
  #                                       updateToThisDate,
  #                                       28, 7, nFirstCols=3,
  #                                       tibbleName="US_Confirmed_Cumulative")
  # US_State_Confirmed_A7 <<- movingAverageData(US_State_Confirmed_Cumulative,
  #                                             updateToThisDate,
  #                                             28, 7, nFirstCols=3,
  #                                             tibbleName="US_State_Confirmed_Cumulative")
  US_County_Confirmed_A7 <<- movingAverageData(US_County_Confirmed_Cumulative,
                                               updateToThisDate,
                                               28, 7, nFirstCols=3,
                                               tibbleName="US_County_Confirmed_Cumulative")
}

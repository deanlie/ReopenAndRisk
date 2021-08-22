source("./mostRecentDataDate.R")
source("./updateTimeSeriesDataFilesAsNecessary.R")
source("./computeNewAndGrowth.R")


loadUSDeathsData <- function() {
  updateToThisDate <- expectedLatestUpdateDataDate()
  updateTimeSeriesDataFilesAsNecessary()
  
  AllDeathsData <- loadATypeOfData("US_Deaths", "US_State_Deaths", "US_County_Deaths", "Deaths")

  US_Deaths_Cumulative <<- AllDeathsData$US_C
  US_State_Deaths_Cumulative <<- AllDeathsData$State_C
  
  US_Deaths_Cumulative_A7 <<- AllDeathsData$US_C_A
  US_State_Deaths_Cumulative_A7 <<- AllDeathsData$State_C_A
  
  testing <- TRUE
  
  US_County_Deaths_Cumulative <<- read_csv("./DATA/US_County_Deaths.csv",
                                           col_types=cols(.default = col_double(),
                                                          Admin2 = col_character(),
                                                          Province_State = col_character(),
                                                          Combined_Key = col_character()))

  if (testing) {
    US_Deaths_New <- AllDeathsData$US_N
    US_State_Deaths_New <- AllDeathsData$State_N

    US_Deaths_New_A7 <- AllDeathsData$US_N_A
    US_State_Deaths_New_A7 <- AllDeathsData$State_N_A
  } else {
    US_Deaths_New <<- computeNewOnDayAndGrowthRate(US_Deaths_Cumulative,
                                                   updateToThisDate,
                                                   nDays = 28, nFirst = 3,
                                                   getGrowthRate = FALSE,
                                                   nonzeroOnly = FALSE,
                                                   tibbleName = "US_Deaths_Cumulative")
    US_State_Deaths_New <<- computeNewOnDayAndGrowthRate(US_State_Deaths_Cumulative,
                                                         updateToThisDate,
                                                         nDays = 28, nFirst = 3,
                                                         getGrowthRate = FALSE,
                                                         nonzeroOnly = FALSE,
                                                         tibbleName = "US_State_Deaths_Cumulative")
    # US_Deaths_Cumulative_A7 <<- movingAverageData(US_Deaths_Cumulative,
    #                                               updateToThisDate,
    #                                               28, 7, nFirstCols=3,
    #                                               tibbleName="US_Deaths_Cumulative")
    # US_State_Deaths_Cumulative_A7 <<- movingAverageData(US_State_Deaths_Cumulative,
    #                                                     updateToThisDate,
    #                                                     28, 7, nFirstCols=3,
    #                                                     tibbleName="US_State_Deaths_Cumulative")
    US_Deaths_New_A7 <<- movingAverageGrowth(US_Deaths_Cumulative,
                                             updateToThisDate,
                                             28, 7, nFirstCols=3,
                                             tibbleName="US_Deaths_Cumulative")
    US_State_Deaths_New_A7 <<- movingAverageGrowth(US_State_Deaths_Cumulative,
                                                   updateToThisDate,
                                                   28, 7, nFirstCols=3,
                                                   tibbleName="US_State_Deaths_Cumulative")
  }
  
  
  US_County_Deaths_New <<- computeNewOnDayAndGrowthRate(US_County_Deaths_Cumulative,
                                                        updateToThisDate,
                                                        nDays = 28, nFirst = 3,
                                                        getGrowthRate = FALSE,
                                                        nonzeroOnly = FALSE,
                                                        tibbleName = "US_County_Deaths_Cumulative")
  US_County_Deaths_Cumulative_A7 <<- movingAverageData(US_County_Deaths_Cumulative,
                                                       updateToThisDate,
                                                       28, 7, nFirstCols=3,
                                                       tibbleName="US_County_Deaths_Cumulative")
  US_County_Deaths_New_A7 <<- movingAverageGrowth(US_County_Deaths_Cumulative,
                                                  updateToThisDate,
                                                  28, 7, nFirstCols=3,
                                                  tibbleName="US_County_Deaths_Cumulative")
}

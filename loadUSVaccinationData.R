source("./mostRecentDataDate.R")
source("./updateStateLevelSerializedDataFiles.R")
source("./computeNewAndGrowth.R")

# loadUSVaccinationData <- function() {
#   rem1000 <- function(n) {
#     as.integer(n - 1000 * floor(n / 1000))
#   }
# 
#   updateToThisDate <- today("EST")
# 
#   US_Vaccinations_As_Filed <- read_csv("./DATA/US_Vaccinations.csv",
#                                 col_types = vaccColTypes())
# 
#   US_State_Vaccinations_As_Filed <- read_csv("./DATA/US_State_Vaccinations.csv",
#                                      col_types = vaccColTypes())
# 
#   US_Vaccinations_A7 <<- movingAverageData(US_Vaccinations,
#                                            updateToThisDate,
#                                            28, 7,
#                                            tibbleName="US_Vaccinations")
#   US_State_Vaccinations_A7 <<- movingAverageData(US_State_Vaccinations,
#                                                  updateToThisDate,
#                                                  28, 7,
#                                                  tibbleName="US_State_Vaccinations")
#   
#   US_Vaccination_Pcts_A7 <<- US_Vaccinations_A7
#   US_State_Vaccination_Pcts_A7 <<- US_State_Vaccinations_A7
# }

loadUSVaccinationData <- function(traceThisRoutine = FALSE, prepend = "") {
  #####################################
  #  Functions local to this routine  #
  #####################################
  readLeaf <- function(aLeaf, colTypes) {
    aPath <- paste("./DATA/", aLeaf, sep = "")
    aTibble <- read_csv(aPath, col_types = colTypes) %>%
      filter(!str_detect(Combined_Key, "Princess"))
  }

  rem1000 <- function(n) {
    as.integer(n - 1000 * floor(n / 1000))
  }

  ######################################
  #  Variables per "loadATypeOfData"   #
  ######################################
  theType <- "Vaccinations"
  computeCounty <- FALSE
  computeNew <- FALSE
  computeAvg <- TRUE
  computePercent <- TRUE
  
  colTypes <- vaccColTypes()

  ######################################
  #      Mainline of  this routine     #
  ######################################
 
  myPrepend = paste("  ", prepend, sep = "")  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered loadUSVaccinationData\n")
  }
  updateToThisDate <- expectedLatestUpdateDataDate()
  updateTimeSeriesDataFilesAsNecessary()
  
  US_leaf <- paste("US_", theType, ".csv", sep = "")
  State_leaf <- paste("US_State_", theType, ".csv", sep = "")
  if (computeCounty) {
    County_leaf <- paste("US_County_", theType, ".csv", sep = "")
  } else {
    County_leaf <- NA
  }
  
  US_Cumulative <- readLeaf(US_leaf, colTypes)
  State_Cumulative <- readLeaf(State_leaf, colTypes)
  if (computeCounty) {
    County_Cumulative <- readLeaf(County_leaf, "County", myCountyTSColTypes())
    if (traceThisRoutine) {
      names_p <- paste(names(County_Cumulative)[1:5])
      cat(file = stderr(), myPrepend, "County_Cumulative names:", names_p, "...\n")
    }
  } else {
    County_Cumulative <- NULL
  }

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Before if(computeNew) block\n")
  }
  
  if (computeNew) {
    US_New <- newFromCumulative(US_Cumulative, updateToThisDate,
                                "US", theType,
                                traceThisRoutine = traceThisRoutine,
                                prepend = myPrepend)
    State_New <- newFromCumulative(State_Cumulative, updateToThisDate,
                                   "State", theType,
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
    
    # dumpLocaleData(State_New, "State_New")
    
    if (computeCounty) {
      County_New <- newFromCumulative(County_Cumulative, updateToThisDate,
                                      "County", theType,
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      if (traceThisRoutine) {
        names_p <- paste(names(County_New)[1:5])
        cat(file = stderr(), myPrepend, "County_New names:", names_p, "...\n")
      }
    } else {
      County_New <- NULL
    }
  } else {
    US_New <- NULL
    State_New <- NULL
    County_New <- NULL
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "Before if(computeAvg) block\n")
  }
  
  if (computeAvg) {
    US_Cumulative_A7 <- movingAverageData(US_Cumulative, updateToThisDate, 28, 7,
                                          tibbleName = paste("US", theType, "Cumulative", sep=""),
                                          traceThisRoutine = traceThisRoutine,
                                          prepend = myPrepend)

    State_Cumulative_A7 <- movingAverageData(State_Cumulative, updateToThisDate, 28, 7,
                                             tibbleName = paste("State", theType, "Cumulative", sep=""),
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)
    
    # dumpLocaleData(State_Cumulative_A7, "State_Cumulative_A7")
    
    if (computeCounty) {
      County_Cumulative_A7 <- movingAverageData(County_Cumulative, updateToThisDate, 28, 7,
                                                tibbleName = paste("County", theType, "Cumulative", sep=""),
                                                traceThisRoutine = traceThisRoutine,
                                                prepend = myPrepend)
      if (traceThisRoutine) {
        names_p <- paste(names(County_Cumulative_A7)[1:5])
        cat(file = stderr(), myPrepend, "County_Cumulative_A7 names:", names_p, "...\n")
      }
    } else {
      County_Cumulative_A7 <- NULL
    }
    
    if (computeNew) {
      US_G7 <- movingAverageGrowth(US_Cumulative, updateToThisDate, 28, 7,
                                   tibbleName = paste("US", theType, "Cumulative", sep = ""),
                                   traceThisRoutine = traceThisRoutine,
                                   prepend = myPrepend)
      
      State_G7 <- movingAverageGrowth(State_Cumulative, updateToThisDate, 28, 7,
                                      tibbleName = paste("State", theType, "Cumulative", sep = ""),
                                      traceThisRoutine = traceThisRoutine,
                                      prepend = myPrepend)
      
      # dumpLocaleData(State_G7, "State_G7")
      
      if (computeCounty) {
        County_G7 <- movingAverageGrowth(County_Cumulative, updateToThisDate, 28, 7,
                                         tibbleName = paste("County", theType, "Cumulative", sep = ""),
                                         traceThisRoutine = traceThisRoutine,
                                         prepend = myPrepend)
        if (traceThisRoutine) {
          names_p <- paste(names(County_G7)[1:5])
          cat(file = stderr(), myPrepend, "County_G7 names:", names_p, "...\n")
        }
      } else {
        County_G7 <- NULL
      }
    } else {
      US_G7 <- NULL
      State_G7 <- NULL
      County_G7 <- NULL
    }
  } else {
    US_Cumulative_A7 <- NULL
    State_Cumulative_A7 <- NULL
    County_Cumulative_A7 <- NULL
    US_G7 <- NULL
    State_G7 <- NULL
    County_G7 <- NULL
    US_G7 <- NULL
    State_G7 <- NULL
    County_G7 <- NULL
  }
  
  if (computePercent) {
    pop_Data <- US_Population %>%
      mutate(FIPSREM = rem1000(FIPS), .keep = "all") %>% # New column is remainder of FIPS / 1000
      filter(FIPSREM == 0) %>%                           # This selects US & States
      select(-FIPSREM) %>%                               # Discard that column
      select(Combined_Key, Population)
  
    US_PopCumulative <- inner_join(pop_Data, US_Cumulative, by = "Combined_Key")
    State_PopCumulative <- inner_join(pop_Data, State_Cumulative, by = "Combined_Key")

    usDims = dim(US_PopCumulative)
    usStDm = dim(State_PopCumulative)
  
    US_CumulativePcts <- US_PopCumulative %>%
      mutate(across(matches(".*2.$"), ~ 100.0 * .x / Population))
    State_CumulativePcts <- State_PopCumulative %>%
      mutate(across(matches(".*2.$"), ~ 100.0 * .x / Population))
  
    getNAvgs <- 28

    US_CumulativePcts_A7 <- movingAverageData(US_CumulativePcts,
                                              updateToThisDate,
                                              getNAvgs, 7,
                                              tibbleName="US_CumulativePcts",
                                              traceThisRoutine = traceThisRoutine,
                                              prepend = myPrepend)
    State_CumulativePcts_A7 <- movingAverageData(State_CumulativePcts,
                                                    updateToThisDate,
                                                    getNAvgs, 7,
                                                    tibbleName="State_CumulativePcts",
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
  } else {
    US_CumulativePcts <- NULL
    State_CumulativePcts <- NULL
    US_CumulativePcts_A7 <- NULL
    State_CumulativePcts_A7 <- NULL
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving loadUSVaccinationData\n")
  }
  
  US_Vaccination_Pcts <<- US_CumulativePcts
  US_State_Vaccination_Pcts <<- State_CumulativePcts
  US_Vaccination_Pcts_A7 <<- US_CumulativePcts_A7
  US_State_Vaccination_Pcts_A7 <<- State_CumulativePcts_A7
  
  list(US_C = US_Cumulative, US_N = US_New,
       US_C_A = US_Cumulative_A7, US_N_A = US_G7, US_G = US_G7,
       State_C = State_Cumulative, State_N = State_New,
       State_C_A = State_Cumulative_A7, State_N_A = State_G7, State_G = State_G7,
       County_C = County_Cumulative, County_N = County_New,
       County_C_A = County_Cumulative_A7, County_N_A = County_G7, County_G = County_G7,
       US_P100 = US_CumulativePcts, State_P100 = State_CumulativePcts,
       US_P100A7 = US_CumulativePcts_A7, State_P100A7 = State_CumulativePcts_A7)
}

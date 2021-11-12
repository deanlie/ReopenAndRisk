# Rebuild US and State data files for US_State_Total_Test_Results.csv data file

# Daily update data is still available in ReopenAndRisk/DATA/mm-dd-2021.csv
# from 04-01-2021 through 07-21-2021

library(tidyverse)
library(curl)

source("dateFormatRoutines.R")
source("columnUtilities.R")

localDataDirectory <- function() {
  "./DATA/"
}

dataPathForDate <- function(aDate) {
  paste(localDataDirectory(), jhuFileDateString(aDate), ".csv",
        sep = "")
}

cleanmmdd2021Vector <- function(aVector) {
  res <- rep(NA, times=length(aVector))
  for (i in 1:length(aVector)) {
    matches <- str_match(aVector[i], "^0?([1-9][0-9]?)-0?([1-9][0-9]?)-2021")
    if (length(matches) == 3) {
      res[i] <- paste(matches[2], "/", matches[3], "/2021", sep="")
    }
  }
  res
}

testcleanmm_int <- function(aDate) {
  dirtyDate <- jhuFileDateString(aDate)
  cleanDate <- cleanmmdd2021Vector(dirtyDate)
  print(paste("dirtyDate: ", dirtyDate, ", cleanDate: ", cleanDate))
}

testCleanmm_etc <- function() {
  testcleanmm_int(as.Date("2021-03-31"))
  testcleanmm_int(as.Date("2021-04-01"))
  testcleanmm_int(as.Date("2021-09-30"))
  testcleanmm_int(as.Date("2021-10-01"))
}

sumIgnoreNA <- function(x) {
  sum(x, na.rm = TRUE)
}

# Data available in the JHU files:

# ************************************************************************************
# The following columns are constant through all dates:
#   Province_State, Country_Region, Lat, Long_, FIPS, UID, ISO3

# ************************************************************************************
# The following column is the same for all rows:
#   Last_Update

# ************************************************************************************
# All data in these columns is NA
#   People_Hospitalized, Hospitalization_Rate

# ************************************************************************************
# Data of interest:
#   Confirmed, Deaths, Recovered, Active, Incident_Rate, Total_Test_Results,
#   Case_Fatality_Ratio, Testing_Rate

# Of which:
#   Available, with breakdown by county, in another file:
#     Confirmed, Deaths

#   Computable from population and another column: ??
#     Incident_Rate, Case_Fatality_Ratio, Testing_Rate
# Hypothesis: Incident_Rate <- Confirmed / (Population / 100000) -- mostly good, up to 35% off!!
# Hypothesis: Case_Fatality_Ratio <- Deaths / Confirmed -- excellent agreement
# Hypothesis: Testing_Rate ? Total_Test_Results / ???

#   Basic data:
#     Recovered,Active,Total_Test_Results

# Hypothesis: People_Hospitalized and Hospitalization_Rate are not being reported
checkDateForHospitalData <- function(fileDate) {
  dataFilePath <- dataPathForDate(fileDate)
  newStateDataTibble <- read_csv(dataFilePath,
                                 col_types = dataFileColTypes()) %>% 
    select(Province_State, Last_Update, People_Hospitalized, Hospitalization_Rate) %>%
    filter(!str_detect(Province_State, "Princess")) %>%
    filter(!(is.na(People_Hospitalized) && is.na(Hospitalization_Rate)))
}

# OK, this returned an empty tibble after checking all data files
checkForHospitalData <- function() {
  nDates = 60
  firstDate <- Sys.Date() - nDates

  buildingHospDataTibble <- tibble(Province_State = c(NA), Last_Update = c(NA),
                                   People_Hospitalized = c(NA), Hospitalization_Rate = c(NA))
  for (i in 0:(nDates - 1)) { # 0:59
    columnDate <- firstDate + i
    
    if (columnDate >= Sys.Date()) {
      print(paste("Up to", as.character(Sys.Date()), "with i", i))
      break;
    }
    
    newData <- checkDateForHospitalData(columnDate)
    buildingHospDataTibble <- bind_rows(buildingHospDataTibble, newData)
  }
  buildingHospDataTibble
}

collectDateConstantColumns <- function(stateName, fileDate) {
  dataFilePath <- dataPathForDate(fileDate)
  newStateDataTibble <- read_csv(dataFilePath,
                                 col_types = dataFileColTypes()) %>% 
    select(Province_State, Country_Region, Lat, Long_, FIPS, UID, ISO3) %>%
    filter(Province_State == !!stateName)
}

collectConstantColumnsForState <- function(stateName) {
  nDates = 60
  firstDate <- Sys.Date() - nDates

  buildingConstDataTibble <- tibble(Province_State = NA, Country_Region = NA,
                                    Lat = NA, Long_ = NA, FIPS = NA,
                                    UID = NA, ISO3 = NA)
  
  for (i in 0:(nDates - 1)) { # 0:59
    columnDate <- firstDate + i
    
    if (columnDate >= Sys.Date()) {
      print(paste("Up to", as.character(Sys.Date()), "with i", i))
      break;
    }
    
    newData <- collectDateConstantColumns(stateName, columnDate)
    buildingConstDataTibble <- bind_rows(buildingConstDataTibble, newData)
  }
  buildingConstDataTibble
}

# ************************************************************************************
# Data of interest:
#   Confirmed, Deaths, Recovered, Active, Incident_Rate, Total_Test_Results,
#   Case_Fatality_Ratio, Testing_Rate

# Of which:
#   Available, with breakdown by county, in another file:
#     Confirmed, Deaths

#   Computable from population and another column: ??
#     Incident_Rate, Case_Fatality_Ratio, Testing_Rate
# Hypothesis: Incident_Rate <- Confirmed / (Population / 100000)
# Hypothesis: Case_Fatality_Ratio <- Deaths / Confirmed
# Hypothesis: Testing_Rate <- Total_Test_Results / Population * 100000
#
# Running checkComputableForDateRange(below) I found that except for the
# areas with smallest population, all those hypothetical computations come
# out within 0.25% for recent data (July 13-20). Here are the exceptions:
#
# Combined_Key                 Inc_Rate_Quality CFR_Quality TTR_Quality
#
# 1 Wyoming, US                             1.00         1.01       1    
# 2 American Samoa, US                    NaN          NaN          1.13 
# 3 Guam, US                                0.975        1          0.975
# 4 Northern Mariana Islands, US            1.07         1          1.07 
# 5 Virgin Islands, US                      1.01         1          1.01 

checkComputableForDate <- function(fileDate, MaxError = 0.01) {
  popFileName <- "US_Population.csv"
  buildingStateTibble <- read_csv(paste(localDataDirectory(), popFileName, sep = ""),
                                  col_types = populationColTypes()) %>%
    filter(Province_State == CountyName) %>%
    filter(Combined_Key != "US") %>%
    select(Combined_Key, Population) %>%
    filter(!str_detect(Combined_Key, "bia,Dis")) %>%
    filter(!str_detect(Combined_Key, "Princess")) %>%
    filter(!str_detect(Combined_Key, "Recovered"))

  dataFilePath <- dataPathForDate(fileDate)
  newStateDataTibble <- read_csv(dataFilePath,
                                 col_types = dataFileColTypes()) %>% 
    select(Province_State, Confirmed, Deaths, Total_Test_Results,
           Incident_Rate, Case_Fatality_Ratio, Testing_Rate) %>%
    mutate(Combined_Key = paste(Province_State, ", US", sep = ""), .before = Province_State, .keep = "unused")
  
  withAdditions <- left_join(buildingStateTibble, newStateDataTibble, by = "Combined_Key") %>%
    mutate(Computed_IncRate = Confirmed / Population * 100000, .after = Incident_Rate, .keep = "all") %>%
    mutate(Computed_CFR = (Deaths * 100)/Confirmed, .after = Case_Fatality_Ratio, .keep = "all") %>%
    mutate(Computed_TTR = Total_Test_Results / Population * 100000, .keep = "all", .after = Testing_Rate)

  LoThresh <- 1.0 - MaxError
  HiThresh <- 1.0 + MaxError

  checkComputations <- withAdditions %>%
    mutate(Combined_Key = Combined_Key,
           Inc_Rate_Quality = Computed_IncRate/Incident_Rate,
           CFR_Quality = Computed_CFR/Case_Fatality_Ratio,
           TTR_Quality = Computed_TTR/Testing_Rate, .keep = "none")
  
  BigErrors <- checkComputations %>%
    filter((Inc_Rate_Quality < {LoThresh}) | (Inc_Rate_Quality > {HiThresh}) |
             (CFR_Quality < {LoThresh}) | (CFR_Quality > {HiThresh}) |
             (TTR_Quality < {LoThresh}) | (TTR_Quality > {HiThresh}))
  
  nLocales = dim(BigErrors)[1]

  cat(file = stdout(), "Check computed params for fileDate", jhuFileDateString(fileDate), "Allowed Error", MaxError, "\n")
  
  if (nLocales > 0) {
    cat(file = stdout(), "Computation/Data mismatch in", nLocales, "locale(s) in data for",
        jhuFileDateString(fileDate), "\n")
    cat(file = stdout(), paste(BigErrors$Combined_Key), "\n\n")
  } else {
    cat(file = stdout(), "No computation/Data mismatches in", nLocales, "in data for",
        jhuFileDateString(fileDate), "\n\n")
  }

  # forCheckTestRate <- withAdditions %>%
  #   select(Combined_Key, Population, Total_Test_Results, Testing_Rate) %>%
  #   mutate(Check = Total_Test_Results / Population * 100000, .keep = "all", .after = Testing_Rate) %>%
  #   mutate(TR_Quality = Check / Testing_Rate, .keep = "all", .after = Check)
  list(allDiffs = checkComputations, smallDiffs = BigErrors, nLocales = nLocales)
}

checkComputableForDateRange <- function(startDateString, stopDateString, MaxError = 0.01) {
  cat(file = stdout(), "Entered checkComputableForDateRange\n")
  startDate <- as.Date(startDateString)
  stopDate <- as.Date(stopDateString)
  
  fileDate <- startDate
  while (fileDate <= stopDate) {
    foo <- checkComputableForDate(fileDate, MaxError)
    fileDate <- fileDate + 1
  }
}
  
rebuildTotalTestResultsFile <- function(traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered rebuildTotalTestResultsFile\n")
  }
  # First, let's be sure we have the dates in the format we need
  nDates = 60
  firstDate <- Sys.Date() - nDates
  
  buildingStateTibble <- read_csv(paste(localDataDirectory(), "US_State_Total_Test_Results.csv", sep = ""),
                                  col_types = myTSColTypes()) %>%
    select(Combined_Key, Population) %>%
    filter(!str_detect(Combined_Key, "Princess")) %>%
    filter(!str_detect(Combined_Key, "Recovered"))
  
  buildingUSTibble <- read_csv(paste(localDataDirectory(), "US_Total_Test_Results.csv", sep = ""),
                               col_types = myTSColTypes()) %>%
    select(Combined_Key, Population)
    
  for (i in 0:(nDates - 1)) { # 0:59
    columnDate <- firstDate + i

    if (columnDate >= Sys.Date()) {
      print(paste("Up to", as.character(Sys.Date()), "with i", i))
      break;
    }

    dataFilePath <- dataPathForDate(columnDate)
    columnName <- cleanmmdd2021Vector(jhuFileDateString(columnDate))
    
    newStateDataTibble <- read_csv(dataFilePath,
                              col_types = dataFileColTypes()) %>%
      select(Province_State, Total_Test_Results) %>%
      filter(!str_detect(Province_State, "Princess")) %>%
      mutate(Combined_Key = paste(Province_State, ", US", sep = ""),
             .keep = "all", .before = Total_Test_Results)

    newUSDataTibble <- newStateDataTibble %>%
      filter(!is.na(Total_Test_Results)) %>%
      summarise(Combined_Key = "US", Total_Test_Results = sum(Total_Test_Results))
    
    # I couldn't figure out how to rename the column with "mutate"
    # ANS:  columnDate <- <new column name>
    #       "{columnDate}" := <old column name>,

    names(newStateDataTibble)[3] <- columnName
    names(newUSDataTibble)[2] <- columnName
    
    # OK, now left_join newStateDataTibble to the Combined_Key/Population base
    buildingStateTibble <- left_join(buildingStateTibble, newStateDataTibble,
                                by = "Combined_Key")
    buildingUSTibble <- left_join(buildingUSTibble, newUSDataTibble,
                                  by = "Combined_Key")
  }

  # Save the results
  write_csv(buildingStateTibble, "./DATA/US_State_Total_Test_Results.csv")
  write_csv(buildingUSTibble, "./DATA/US_Total_Test_Results.csv")

  result <- list(US = buildingUSTibble, States = buildingStateTibble)
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving rebuildTotalTestResultsFile\n")
  }
  
  return(re)
}


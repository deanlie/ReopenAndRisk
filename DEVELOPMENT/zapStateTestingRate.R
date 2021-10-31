findLongestString <- function(stringVector) {
  maxLen <- 0
  maxString <- ""
  for (aString in stringVector) {
    if (str_length(aString) > maxLen) {
      maxLen <- str_length(aString)
      maxString <- aString
    }
  }
  return(maxString)
}

zapXXYYCols <- function(filenameStem) {
  # As of Oct 13, the US_Testing_Rate.csv file was not being updated properly.
  # At that time US_State_Testing_Rate, US_State_Total_Test_Results,
  #   US_State_Case_Fatality_Ratio, and US_State_Incident_Rate were updated
  #   every time the app was started, which resulted in multiple columns headed
  #   e.g. 10/01/21.x.x.x.x etc in those files. At the same time,
  #   US_Testing_Rate, US_Total_Test_Results, US_Case_Fatality_Ratio, and
  #   US_Incident_Rate were never updated beyond 9/30/21.
  
  # Data was present for 10/01/21.x through 10/12/21.x in the "State" files above.
  
  # This routine removes columns with ".x.x" and ".y" in their names and replaces
  #   columns like "10/01/21.x" with "10/1/21"
  
  filePath <- paste("./DATA/", filenameStem, ".csv", sep = "")

  longData <- read_csv(filePath, show_col_types = FALSE)
  
  cat(file = stderr(), "Removing bogus columns from", filePath, "\n")
  cat(file = stderr(),
      "  Longest column was",
      findLongestString(names(longData)),
      "\n")

  goodDataOnce <- longData %>%
    select(-all_of(matches("x\\.x"))) %>%
    select(-all_of(matches("\\.y")))
  
  nextPreceder <- "9/30/20"

  view(goodDataOnce[1:4, c(1:2, 120:132)])
  for (i in 1:28) {
    inz <- as.character(i)
    thisDstCol <- paste("10/", inz, "/20", sep = "")
    thisSrcCol <- paste("10/", inz, "/20.x", sep = "")
    goodDataOnce <- goodDataOnce %>%
      mutate({{thisDstCol}} := .data[[thisSrcCol]], .after = {{nextPreceder}}, .keep = "unused")
    view(goodDataOnce[1:4, c(1:2, 120:132)])
    nextPreceder <- thisDstCol
  }

  write_csv(goodDataOnce, filePath)

  cat(file = stderr(), "Re-wrote", filePath, "\n")
  
  return(goodDataOnce)
}

zap2020Data <- function(filenameStem) {
  filePath <- paste("./DATA/", filenameStem, ".csv", sep = "")
  
  longData <- read_csv(filePath, show_col_types = FALSE)
  goodDataOnce <- longData %>%
    select(-all_of(matches("x\\.x"))) %>%
    select(-all_of(matches("\\.y"))) %>%
    select(-all_of(matches("20$"))) %>%
    select(-all_of(matches("20\\.x")))
  
  write_csv(goodDataOnce, filePath)
  
  cat(file = stderr(), "Re-wrote", filePath, "\n")
  
  return(goodDataOnce)
}

makeSmallSetOfStaticData <- function() {
  US_files <- c("US_Case_Fatality_Ratio.csv",
                "US_Confirmed.csv",
                "US_County_Confirmed.csv",
                "US_County_Deaths.csv",
                "US_Deaths.csv",
                "US_Incident_Rate.csv",
                "US_Testing_Rate.csv",
                "US_Total_Test_Results.csv",
                "US_Vaccinations.csv",
                "US_Population.csv")
  State_Files <- c("US_State_Case_Fatality_Ratio.csv",
                  "US_State_Confirmed.csv",
                  "US_State_Deaths.csv",
                  "US_State_Incident_Rate.csv",
                  "US_State_Population_Est.csv",
                  "US_State_Testing_Rate.csv",
                  "US_State_Total_Test_Results.csv",
                  "US_State_Vaccinations.csv")
  County_Files <- c()

  for (aFile in c("US_State_Case_Fatality_Ratio.csv")) { # Should be "State_Files"
    aPath <- paste("./DATA/STATIC/", aFile, sep = "")
    activeTibble <- read_csv(aPath, show_col_types = FALSE) %>%
      filter(Combined_Key == "Alabama, US" |
               Combined_Key == "Louisiana, US" |
               Combined_Key == "Puerto Rico, US" |
               Combined_Key == "Massachusetts, US" |
               Combined_Key == "Maine, US" |
               Combined_Key == "Florida, US" |
               Combined_Key == "Texas, US")
    x <- 0
  }
  
  return(activeTibble)
}

foo <- makeSmallSetOfStaticData()

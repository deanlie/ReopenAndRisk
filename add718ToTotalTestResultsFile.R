# NEW FILE!
add718ToTotalTestResultsFile <- function() {
  # dataFileColSpec <- cols(.default = col_double(),
  #                         Province_State = col_character(),
  #                         Country_Region = col_character(),
  #                         Last_Update = col_datetime(format = ""),
  #                         Recovered = col_logical(),
  #                         Active = col_logical(),
  #                         People_Hospitalized = col_logical(),
  #                         ISO3 = col_character())
  
  # First, let's be sure we have the dates in the format we need
  nDates = 3
  firstDate <- as.Date("2021-07-18")
  
  zap1 = "7/19/21"
  zap2 = "7/20/21"
  
  buildingStateTibble <- read_csv(paste(theDataDirectory(), "US_State_Total_Test_Results.csv", sep = ""),
                                  col_types = cols(.default = col_double(),
                                                   Province_State = col_character(),
                                                   Combined_Key = col_character())) %>%
    select(-{zap1}, -{zap2}) %>%
    filter(!str_detect(Combined_Key, "Princess")) %>%
    filter(!str_detect(Combined_Key, "Recovered"))
  
  buildingUSTibble <- read_csv(paste(theDataDirectory(), "US_Total_Test_Results.csv", sep = ""),
                               col_types = cols(.default = col_double(),
                                                Province_State = col_character(),
                                                Combined_Key = col_character())) %>%
    select(-{zap1}, -{zap2})
  
  for (i in 0:2) { # 0:2
    columnDate <- firstDate + i
    
    if (columnDate >= Sys.Date()) {
      print(paste("Up to", as.character(Sys.Date()), "with i", i))
      break;
    }
    
    dataFilePath <- dataPathForDate(columnDate)
    columnName <- cleanmmdd2021Vector(jhuFileDateString(columnDate))
    
    # cat(file = stderr(), "Reading", dataFilePath, "\n")
    
    newStateDataTibble <- read_csv(dataFilePath,
                                   col_types = dataFileColSpec()) %>%
      select(Province_State, Total_Test_Results) %>%
      filter(!str_detect(Province_State, "Princess")) %>%
      mutate(Combined_Key = paste(Province_State, ", US", sep = ""),
             .keep = "unused", .before = Total_Test_Results)
    
    newUSDataTibble <- newStateDataTibble %>%
      filter(!is.na(Total_Test_Results)) %>%
      summarise(Combined_Key = "US", Total_Test_Results = sum(Total_Test_Results))
    
    # cat(file = stderr(), "State data names", paste(names(newStateDataTibble)), "\n")
    # cat(file = stderr(), "State data excerpt", paste(newStateDataTibble[1,]), "\n")
    # cat(file = stderr(), "US data names", paste(names(newUSDataTibble)), "\n")
    # cat(file = stderr(), "US data excerpt", paste(newUSDataTibble[1,]), "\n")
    
    # I couldn't figure out how to rename the column with "mutate"
    names(newStateDataTibble)[2] <- columnName
    names(newUSDataTibble)[2] <- columnName
    
    # cat(file = stderr(), "State data names(2)", paste(names(newStateDataTibble)), "\n")
    # cat(file = stderr(), "US data names(2)", paste(names(newUSDataTibble)), "\n")
    
    # OK, now left_join newStateDataTibble to the Combined_Key/Population base
    buildingStateTibble <- left_join(buildingStateTibble, newStateDataTibble,
                                     by = "Combined_Key")
    buildingUSTibble <- left_join(buildingUSTibble, newUSDataTibble,
                                  by = "Combined_Key")
  }
  
  # Save the results
  write_csv(buildingStateTibble, paste(theDataDirectory(), "US_State_Total_Test_Results3.csv", sep = ""))
  write_csv(buildingUSTibble, paste(theDataDirectory(), "US_Total_Test_Results3.csv", sep = ""))
}

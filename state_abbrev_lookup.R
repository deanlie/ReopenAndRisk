# Lookup table to get state names from 2-letter post office abbreviations

stateLookup2DC <- c("AK"="Alaska", "AL"="Alabama", "AR"="Arkansas", "AZ"="Arizona", "CA"="California", 
                 "CO"="Colorado", "CT"="Connecticut", "DC"="District of Columbia, District of Columbia",
                 "DE"="Delaware", "FL"="Florida", "GA"="Georgia", "HI"="Hawaii", "IA"="Iowa", "ID"="Idaho",
                 "IL"="Illinois", "IN"="Indiana", "KS"="Kansas", "KY"="Kentucky", "LA"="Louisiana",
                 "MA"="Massachusetts", "MD"="Maryland", "ME"="Maine", "MI"="Michigan", "MN"="Minnesota",
                 "MO"="Missouri", "MS"="Mississippi", "MT"="Montana", "NC"="North Carolina",
                 "ND"="North Dakota", "NE"="Nebraska", "NH"="New Hampshire", "NJ"="New Jersey",
                 "NM"="New Mexico", "NV"="Nevada", "NY"="New York", "OH"="Ohio", "OK"="Oklahoma",
                 "OR"="Oregon", "PA"="Pennsylvania", "PR"="Puerto Rico", "RI"="Rhode Island",
                 "SC"="South Carolina", "SD"="South Dakota", "TN"="Tennessee", "TX"="Texas", 
                 "UT"="Utah", "VA"="Virginia", "VI"="Virgin Islands", "VT"="Vermont", "WA"="Washington", 
                 "WI"="Wisconsin", "WV"="West Virginia", "WY"="Wyoming")

stateLookup <- c("AK"="Alaska", "AL"="Alabama", "AR"="Arkansas", "AZ"="Arizona", "CA"="California", 
                    "CO"="Colorado", "CT"="Connecticut", "DC"="District of Columbia", "DE"="Delaware",
                    "FL"="Florida", "GA"="Georgia", "HI"="Hawaii", "IA"="Iowa", "ID"="Idaho",
                    "IL"="Illinois", "IN"="Indiana", "KS"="Kansas", "KY"="Kentucky", "LA"="Louisiana",
                    "MA"="Massachusetts", "MD"="Maryland", "ME"="Maine", "MI"="Michigan", "MN"="Minnesota",
                    "MO"="Missouri", "MS"="Mississippi", "MT"="Montana", "NC"="North Carolina",
                    "ND"="North Dakota", "NE"="Nebraska", "NH"="New Hampshire", "NJ"="New Jersey",
                    "NM"="New Mexico", "NV"="Nevada", "NY"="New York", "OH"="Ohio", "OK"="Oklahoma",
                    "OR"="Oregon", "PA"="Pennsylvania", "PR"="Puerto Rico", "RI"="Rhode Island",
                    "SC"="South Carolina", "SD"="South Dakota", "TN"="Tennessee", "TX"="Texas", 
                    "UT"="Utah", "VA"="Virginia", "VI"="Virgin Islands", "VT"="Vermont", "WA"="Washington", 
                    "WI"="Wisconsin", "WV"="West Virginia", "WY"="Wyoming")

admin1TypeFor <- function(aStateChoice) {
  admin1T_UC_S  <- "County"
  admin1T_UC_PL <- "Counties"
  admin1T_LC_S  <- "county"
  admin1T_LC_PL <- "counties"
  if ((!is_null(aStateChoice)) && aStateChoice == "LA") {
    admin1T_UC_S  <- "Parish"
    admin1T_UC_PL <- "Parishes"
    admin1T_LC_S  <- "parish"
    admin1T_LC_PL <- "parishes"
  }
  if ((!is_null(aStateChoice)) && aStateChoice == "PR") {
    admin1T_UC_S  <- "Municipio"
    admin1T_UC_PL <- "Municipios"
    admin1T_LC_S  <- "municipio"
    admin1T_LC_PL <- "municipios"
  }
  list(UC_S = admin1T_UC_S, UC_PL = admin1T_UC_PL,
       LC_S = admin1T_LC_S, LC_PL = admin1T_LC_PL)
}

makeCombinedKeys <- function(theCounty, stateAbbrev) {
  theLocaleKey <- "US"
  theSpacelessLocaleKey <- "US"
  
  if ((!is_null(stateAbbrev)) &&!is.na(stateAbbrev)) {
    theState <- unname(stateLookup[stateAbbrev])
    theLocaleKey <- paste(theState, ", ", theLocaleKey, sep="")
    theSpacelessLocaleKey <- paste(theState, ",",
                                   theSpacelessLocaleKey, sep="")
    if ((!is_null(theCounty)) && ! is.na(theCounty)) {
      theLocaleKey <- paste(theCounty, ", ", theLocaleKey, sep="")
      theSpacelessLocaleKey <- paste(theCounty, ",",
                                     theSpacelessLocaleKey, sep="")
    }
  }
  list(spaced = theLocaleKey, spaceless = theSpacelessLocaleKey)
}




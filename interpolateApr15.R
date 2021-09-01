# Interpolate in Apr 15 version of US_Vaccinations.csv and US_State_Vaccinations.csv

source("columnUtilities.R")

# Read in the data file
# Interpolate in the resulting tibble
# Write the data file

# Replace one column with linear interpolation of those on its sides
# colBefore, colN, colAfter arguments are the column names as strings
# colN is the one we're replacing
replaceOneColWithInterpolation <- function (aTibble, colBefore, colN, colAfter) {
  newTibble <- aTibble %>%
    mutate("{colBefore}" := .data[[colBefore]],
           "{colN}" := (.data[[colBefore]] + .data[[colAfter]]) / 2.0,
           "{colAfter}" := .data[[colAfter]],
           .keep = "unused")
}

# Insert two columns with linear interpolation
#   (2 parts A, 1 part B, then 1 part A, 2 parts B)
#   of those on its sides
# colBefore, colN1, colN2, colAfter arguments are the column names as strings
# colN1 and colN2 are the ones being inserted
insertTwoColsAsInterpolated <- function (aTibble, colBefore, colN1, colN2, colAfter) {
  newTibble <- aTibble %>%
    mutate("{colBefore}" := .data[[colBefore]],
           "{colN1}" := (.data[[colBefore]] * 2 + .data[[colAfter]]) / 3.0,
           "{colN2}" := (.data[[colBefore]] + .data[[colAfter]] * 2) / 3.0,
           "{colAfter}" := .data[[colAfter]],
           .keep = "unused",
           .after = .data[[colBefore]])
}

fixDataThruApr15 <- function(aTibble) {
  newTibble <- aTibble %>%
    replaceOneColWithInterpolation("4/7/21", "4/8/21", "4/9/21") %>%
    insertTwoColsAsInterpolated("4/9/21", "4/10/21", "4/11/21", "4/12/21") %>%
    replaceOneColWithInterpolation("4/13/21", "4/14/21", "4/15/21")
}

getTheTransform <- function() {
  # Change this value as necessary! Keep the routine "fixDataThruApr15"
  #   as an example
  return(fixDataThruApr15)
}

readMungeWrite <- function(pathIn, theColTypes, pathOut, theTransform) {
  read_csv(pathIn, col_types = theColTypes) %>%
    theTransform() %>%
    write_csv(pathOut)
}


# ################

readMungeWrite("DATA/US_Vaccinations.csv",
               vaccColTypes(),
               "DATA/US_Vaccinations_MG.csv",
               getTheTransform())

readMungeWrite("DATA/US_State_Vaccinations.csv",
               vaccColTypes(),
               "DATA/US_State_Vaccinations_MG.csv",
               getTheTransform())



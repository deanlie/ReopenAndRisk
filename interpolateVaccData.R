# Interpolate in US_Vaccinations.csv and US_State_Vaccinations.csv

# Read in a data file
# Interpolate in the resulting tibble
# Write the data file

# Replace one column with linear interpolation of those on its sides
# colBefore, colN, colAfter arguments are the column names as strings
# colN is the one we're replacing

replaceOneColWithIntegers <- function(aTibble, colN) {
  newTibble <- aTibble %>%
    mutate("{colN}" := round(.data[[colN]]),
           .keep = "unused")
}

replaceOneColWithInterpolation <- function (aTibble, colBefore, colN, colAfter) {
  newTibble <- aTibble %>%
    mutate("{colBefore}" := .data[[colBefore]],
           "{colN}" := round((.data[[colBefore]] + .data[[colAfter]]) / 2.0),
           "{colAfter}" := .data[[colAfter]],
           .keep = "unused")
}

# Insert one columns with linear interpolation of those on its sides
#   (1 parts A, 1 part B)
# colBefore, colN, colAfter arguments are the column names as strings
# colN is the one being inserted
insertOneColAsInterpolated <- function (aTibble, colBefore, colN, colAfter) {
  newTibble <- aTibble %>%
    mutate("{colN}" := round((.data[[colBefore]] + .data[[colAfter]]) / 2.0),
           .keep = "all",
           .after = .data[[colBefore]])
}

# Insert two columns with linear interpolation of those on its sides
#   (2 parts A, 1 part B, then 1 part A, 2 parts B)
# colBefore, colN1, colN2, colAfter arguments are the column names as strings
# colN1 and colN2 are the ones being inserted
insertTwoColsAsInterpolated <- function (aTibble, colBefore, colN1, colN2, colAfter) {
  newTibble <- aTibble %>%
    mutate("{colN1}" := round((.data[[colBefore]] * 2 + .data[[colAfter]]) / 3.0),
           "{colN2}" := round((.data[[colBefore]] + .data[[colAfter]] * 2) / 3.0),
           .keep = "all",
           .after = .data[[colBefore]])
}

insertThreeColsAsInterpolated <- function (aTibble, colBefore, colN1, colN2, colN3, colAfter) {
  newTibble <- aTibble %>%
    mutate("{colN1}" := round((.data[[colBefore]] * 3 + .data[[colAfter]]) / 4.0),
           "{colN2}" := round((.data[[colBefore]] + .data[[colAfter]]) / 2.0),
           "{colN3}" := round((.data[[colBefore]] + .data[[colAfter]] * 3) / 4.0),
           .keep = "all",
           .after = .data[[colBefore]])
}
insertFourColsAsInterpolated <- function (aTibble, colBefore, colN1, colN2, colN3, colN4, colAfter) {
  newTibble <- aTibble %>%
    mutate("{colN1}" := round((.data[[colBefore]] * 4 + .data[[colAfter]]) / 5.0),
           "{colN2}" := round((.data[[colBefore]] * 3 + .data[[colAfter]] * 2) / 5.0),
           "{colN3}" := round((.data[[colBefore]] * 2 + .data[[colAfter]] * 3) / 5.0),
           "{colN4}" := round((.data[[colBefore]] + .data[[colAfter]] * 4) / 5.0),
           .keep = "all",
           .after = .data[[colBefore]])
}

fixDataThruApr15 <- function(aTibble) {
  newTibble <- aTibble %>%
    replaceOneColWithInterpolation("4/7/21", "4/8/21", "4/9/21") %>%
    insertTwoColsAsInterpolated("4/9/21", "4/10/21", "4/11/21", "4/12/21") %>%
    replaceOneColWithInterpolation("4/13/21", "4/14/21", "4/15/21")
}

integerApr10And11 <- function(aTibble) {
  newTibble <- aTibble %>%
    replaceOneColWithIntegers("4/10/21") %>%
    replaceOneColWithIntegers("4/11/21")
}

fixDataThruApr27 <- function(aTibble) {
  newTibble <- aTibble %>%
    insertTwoColsAsInterpolated("4/18/21", "4/19/21", "4/20/21", "4/21/21") %>%
    insertTwoColsAsInterpolated("4/23/21", "4/24/21", "4/25/21", "4/26/21")
}

fixDataThruApr29 <- function(aTibble) {
  newTibble <- aTibble %>%
    insertOneColAsInterpolated("4/27/21", "4/28/21", "4/29/21")
}

fixDataThruMay6 <- function(aTibble) {
  newTibble <- aTibble %>%
    insertTwoColsAsInterpolated("5/1/21", "5/2/21", "5/3/21", "5/4/21")
}

dropMay6 <- function(aTibble) {
  newTibble <- aTibble %>%
    select(-"5/6/21")
}

fixDataThruMay11 <- function(aTibble) {
  newTibble <- aTibble %>%
    insertOneColAsInterpolated("5/7/21", "5/9/21", "5/11/21") %>%
    insertOneColAsInterpolated("5/7/21", "5/8/21", "5/9/21") %>%
    insertOneColAsInterpolated("5/9/21", "5/10/21", "5/11/21")
}

fixDataThruMay19 <- function(aTibble) {
  newTibble <- aTibble %>%
    insertFourColsAsInterpolated("5/14/21", "5/15/21", "5/16/21", "5/17/21", "5/18/21", "5/19/21")
}

# Change this assignment as necessary! Keep the routines "fixDataThruApr15" etc.
#   as examples
mungeData <- fixDataThruMay19

readMungeWrite <- function(pathIn, pathOut) {
  read_csv(pathIn,
           col_types = cols(.default = col_double(),
                            Combined_Key = col_character(),
                            Datum = col_character(),
                            Loc_Datum = col_character())) %>%
    mungeData() %>%
    write_csv(pathOut)
}


# ################

readMungeWrite("DATA/US_Vaccinations.csv", "DATA/US_Vaccinations_MG.csv")

readMungeWrite("DATA/US_State_Vaccinations.csv", "DATA/US_State_Vaccinations_MG.csv")



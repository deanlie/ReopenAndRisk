cleanDataForPresentation <- function(theData,
                                     stateChoices,
                                     countyChoices) {
  if (is.null(countyChoices) || length(countyChoices) == 0) {
    theData <- theData %>%
      mutate(State = Province_State, .before = 1, .keep = "unused") %>%
      select(-Combined_Key, -Population)
  } else {
    admin1 <- admin1TypeFor(stateChoices[1])$UC_S
    theData <- theData %>%
      mutate({{admin1}} := Admin2,
             .before = 1, .keep = "unused") %>%
      select(-Combined_Key, -Population, -Province_State)
  }
  return(theData)
}

makeGtPresentation <- function(theData, stateChoices, countyChoices,
                               theTitle, theSubtitle) {
  # write_csv(theData, "../ShinyPart1/presentThis.csv")
  # cat(theTitle, "\n", file = "../ShinyPart1/aTitle.txt")
  
  theData %>%
    gt() %>%
    tab_header(title = theTitle,
               subtitle = theSubtitle) %>%
    fmt_number(columns = matches("^[1-9]"), decimals = 1) %>%
    tab_style(
      style = list(cell_text(size = "small")),
      locations = cells_body()) %>%
    tab_style(
      style = list(cell_text(size = "small")),
      locations = cells_column_labels())
}

styleSelectedLines <- function(theData, stateChoices, countyChoices) {
  return(theData)
}

bogusGtDisplay <- function(caller = "unimplemented routine") {
  theDataFrame <- data.frame(list(A = c(16, 5, 9, 4),
                                  B = c(3, 10, 6, 15),
                                  C = c(2, 11, 7, 14),
                                  D = c(13, 8, 12, 1)))
  
  theData <- as.tibble(theDataFrame) %>%
    gt() %>%
    tab_header(title = paste("PLACEHOLDER for", caller, sep = " "), 
               subtitle = "Magic square from Albrecht Durer's 'Melancholia'") %>%
    cols_label(A = "", B = "", C = "", D = "") %>%
    fmt_number(columns = matches("^[1-9]"), decimals = 1) %>%
    tab_style(
      style = list(cell_text(size = "small")),
      locations = cells_body()) %>%
    tab_style(
      style = list(cell_text(size = "small")),
      locations = cells_column_labels()) %>%
    tab_style(
      style = list(cell_text(color = "red")),
      locations = (cells_title()) 
    )
}

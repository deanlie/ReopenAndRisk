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

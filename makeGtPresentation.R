cleanDataForPresentation <- function(theData,
                                     stateChoices,
                                     countyChoices,
                                     traceThisRoutine = FALSE,
                                     prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered cleanDataForPresentation\n")
  }

  theNames <- names(theData)
  if (is.null(countyChoices) || is.null(stateChoices)) {
    theData <- theData %>%
      mutate(State = str_replace(Combined_Key, ", US", ""), .before = 1, .keep = "unused")
  } else {
    admin1 <- admin1TypeFor(stateChoices[1])$UC_S
    selectedState <- stateLookup[stateChoices[1]]
    state_US <- paste(", ", selectedState, ", US", sep = "")
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "state_US match string is '", state_US, "'\n")
    }
    theData <- theData %>%
      mutate({{admin1}} := str_replace(Combined_Key, {{state_US}}, ""),
             .before = 1, .keep = "unused")
    if ("Admin2" %in% theNames) {
      theData <- theData %>%
        select(-Admin2)
    }
  }
  if ("Population" %in% theNames) {
    theData <- theData %>%
      select(-Population)
  }
  if ("Province_State" %in% theNames) {
    theData <- theData %>%
      select(-Province_State)
  }

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving cleanDataForPresentation\n")
  }

  return(theData)
}

makeGtPresentation <- function(theData, stateChoices, countyChoices,
                               theTitle, theSubtitle, theID = "aTable",
                               nDecimals = 1) {
  # write_csv(theData, "../ShinyPart1/presentThis.csv")
  # cat(theTitle, "\n", file = "../ShinyPart1/aTitle.txt")
  
  theData %>%
    gt(id = theID) %>%
    tab_header(title = theTitle,
               subtitle = theSubtitle) %>%
    fmt_number(columns = matches("^[1-9]"), decimals = nDecimals) %>%
    tab_style(
      style = list(cell_text(size = "small")),
      locations = cells_body()) %>%
    tab_style(
      style = list(cell_text(size = "small")),
      locations = cells_column_labels())
}

makeGtPresentationForTab <- function(dataSelectionRtn, movingAvg,
                                     stateChoices, countyChoices,
                                     theTitle, theSubtitle,
                                     theID = "Missing_ID",
                                     nDecimals = 1,
                                     traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered makeGtPresentationForTab\n")
  }

  if (is.null(stateChoices)) {
    theData <- dataSelectionRtn(NULL, movingAvg, stateChoices)
  } else {
    theData <- dataSelectionRtn(countyChoices, movingAvg, stateChoices)
  }
  
  theData <- cleanDataForPresentation(theData,
                                      stateChoices,
                                      countyChoices)
  
  result <- makeGtPresentation(theData, stateChoices, countyChoices,
                               theTitle, theSubtitle,
                               theID, nDecimals = nDecimals) %>%
    styleSelectedLines(stateChoices, countyChoices)
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving makeGtPresentationForTab\n")
  }

  return(result)
}
  

styleSelectedLines <- function(theData, stateChoices, countyChoices) {
  return(theData)
}

bogusGtDisplay <- function(caller = "unimplemented routine") {
  theDataFrame <- data.frame(list(A = c(16, 5, 9, 4),
                                  B = c(3, 10, 6, 15),
                                  C = c(2, 11, 7, 14),
                                  D = c(13, 8, 12, 1)))
  
  placeholderTitle <- function() {
    expectedFailure <- FALSE # Set TRUE to generate reference .json, .png for test
    if (expectedFailure) {
      return("expected FAILURE in")
    } else {
      return("PLACEHOLDER for")      
    }
  }
  
  theData <- as.tibble(theDataFrame) %>%
    gt(id = "bogustbl") %>%
    tab_header(title = paste(placeholderTitle(), caller, sep = " "), 
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

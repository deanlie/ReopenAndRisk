library(tidyverse)

# A Debugging utility
returnEndsOfTibbleRow <- function(aTibble, itsName = "<?>",
                                  theKey = "Combined_Key",
                                  keyValue = NULL,
                                  nFirst = 3, nLast = 8,
                                  optionalMessage = "",
                                  traceThisRoutine = FALSE,
                                  prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered returnEndsOfTibbleRow\n")
    if (optionalMessage != "") {
      cat(file = stderr(), myPrepend, optionalMessage, "\n")
    }
  }
  
  if (is.null(keyValue)) {
    theData <- filter(aTibble,
                      (.data[[theKey]] == "US") |
                        (.data[[theKey]] == "Massachusetts, US") |
                        (.data[[theKey]] == "Suffolk, Massachusetts, US"))
  } else {
    theData <- filter(aTibble, .data[[theKey]] == {keyValue})
  }

  theLength <- dim(theData)[2]
  preEnd <- (theLength + 1) - nLast

  if (traceThisRoutine) {
    nDigits <- getOption("digits")
    options(digits = 6)
    cat(file = stderr(), myPrepend, "Ends of tibble", itsName, "\n")
    cat(file = stderr(), myPrepend, "key =", theKey, "keyValue =", keyValue, "\n")
    cat(file = stderr(), myPrepend, "Length:", theLength, "\n")
    cat(file = stderr(), myPrepend, "First cols:", paste(names(theData)[1:nFirst]), "\n")
    cat(file = stderr(), myPrepend, "First data:", paste(theData[1,1:nFirst]), "\n")
    cat(file = stderr(), myPrepend, "Last cols:", paste(names(theData)[preEnd:theLength]), "\n")
    cat(file = stderr(), myPrepend, "Last data:", paste(as.integer(theData[1,preEnd:theLength])), "\n")
    options(digits = nDigits)
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving returnEndsOfTibbleRow\n")
  }
  
  return(theData[1, c(1:nFirst, preEnd:theLength)])
}

conciseEndsOfTibbleRow <- function(aTibble, itsName = "<?>",
                                   theKey = "Combined_Key",
                                   keyValue = NULL,
                                   nFirst = 2, nLast = 2,
                                   traceThisRoutine = FALSE,
                                   prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered conciseEndsOfTibbleRow\n")
  }
  if (is.null(keyValue)) {
    theData <- filter(aTibble,
                      (.data[[theKey]] == "US") |
                        (.data[[theKey]] == "Massachusetts, US") |
                        (.data[[theKey]] == "Suffolk, Massachusetts, US"))
  } else {
    theData <- filter(aTibble, .data[[theKey]] == {keyValue})
  }

  theLength <- dim(theData)[2]
  preEnd <- (theLength + 1) - nLast
  
  if (traceThisRoutine) {
    nDigits <- getOption("digits")
    options(digits = 6)
    cat(file = stderr(), myPrepend, "Tibble", itsName, "has", theLength, "cols\n")
    cat(file = stderr(), myPrepend, "Cols:",
        paste(names(theData)[c(1:nFirst)]), "...",
        paste(names(theData)[c(preEnd:theLength)]),"\n")
    cat(file = stderr(), myPrepend, "Data:",
        paste(sprintf("%10.2f", theData[1,c(1:nFirst)])), "...",
        paste(sprintf("%10.2f", theData[1,c(preEnd:theLength)])), "\n")
    options(digits = nDigits)
  }
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving conciseEndsOfTibbleRow\n")
  }
  
  return(theData[1, c(1:nFirst, preEnd:theLength)])
}

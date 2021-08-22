# Stand-alone test for stat_boxplot warning

assembleRatioDeltaBoxPlot <- function(numeratorFrame, denominatorFrame,
                                      stateChoices, theTitle, xlabel, ylabel,
                                      clampFactor = 3,
                                      timeWindow = 14,
                                      nFirstNum = 4,
                                      nFirstDenom = 4,
                                      traceThisRoutine = TRUE,
                                      prepend = "CALLER??") {
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleRatioDeltaBoxPlot\n")
  }
  myPrepend <- paste("  ", prepend, sep = "")
  
  myDataFrame <- ratioDeltaFrame(numeratorFrame, denominatorFrame, timeWindow,
                                 nFirstNum = nFirstNum, nFirstDenom = nFirstDenom,
                                 traceThisRoutine, myPrepend)
  res <- computePlotDataDirectFromCumulative(myDataFrame, 
                                             FALSE, # OUCH chooseCounty,
                                             NULL,  # OUCH countyChoices,
                                             stateChoices,
                                             timeWindow, nFirst = 1,
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)
  
  myBreaks <- unique(res$plotData$date)
  dateLabels <- res$dateLabels
  
  clampedList <- clampDataIfRequested(res$plotData, clampFactor)
  plotData    <- clampedList$plotData
  
  palette("ggplot2")
  
  p <- ggplot(data = plotData)
  
  warnOption <- getOption("warn")
  options(warn = -1)
  p <- p + geom_boxplot(mapping = aes(x = date, y = nums), na.rm = TRUE)
  options(warn = warnOption)
  
  p <- p + geom_point(data = filter(plotData, region %in% res$AreasOfInterest),
                      mapping = aes(x = date, y = nums, color = region))
  p <- p + labs(title = theTitle)
  p <- p + xlab(xlabel)
  p <- p + ylab(ylabel)
  p <- p + scale_x_discrete(breaks = myBreaks,
                            labels = dateLabels)
  
  list(thePlot = p,
       diags   = list(theData      = plotData,
                      clampedN     = clampedList$clampedN,
                      boxplotStats = clampedList$bps))
}

plotTestResultBoxplotsX <- function(chooseCounty, movingAvg, countyChoices,
                                   stateChoices, timeWindow) {
  prepend <- ""
  traceThisRoutine <- FALSE
  myPrepend <- paste("  ", prepend, sep = "")
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered plotTestResultBoxplots\n")
  }
  
  # updateDataForUSTypeIfNeeded("Confirmed")
  if (movingAvg) {
    title <- "Test Positivity Distribution, 7 day moving average"
    theCaseData <- US_State_Confirmed_A7
    theTestData <- US_State_People_Tested_A7 
  } else {
    title <- "Test Positivity Distribution"
    theCaseData <- US_State_Confirmed
    theTestData <- US_State_People_Tested 
  }
  
  result <- assembleRatioDeltaBoxPlot(theCaseData, theTestData, stateChoices,
                                      title,
                                      paste("Last", timeWindow, "days"),
                                      "Test Positivity: percent of tests returning positive",
                                      clampFactor = 1,
                                      timeWindow = timeWindow,
                                      nFirstNum = 2, nFirstDenom = 2,
                                      traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving plotTestResultBoxplots\n")
  }
  return(result)
}

runTest <- function() {
  US_State_Confirmed_A7 <<- read_csv("./FOO/US_State_Conf_A7.csv",
                                     col_types = cols(
                                       .default = col_double(),
                                       Province_State = col_character(),
                                       Combined_Key = col_character()
                                     ))
  US_State_People_Tested_A7 <<- read_csv("./FOO/US_State_PT_A7.csv",
                                         col_types = cols(
                                           .default = col_double(),
                                           Combined_Key = col_character()
                                         ))

  result <- plotTestResultBoxplotsX(FALSE, TRUE, NULL, c("MA", "ME"), 14)
  print(result$thePlot)
}

  
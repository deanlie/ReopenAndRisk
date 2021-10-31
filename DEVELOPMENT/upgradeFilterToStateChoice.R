
# filterToStateChoiceD <- function(aTibble, aStateChoice, countyChoices = NULL) {
#   selectedState <- stateLookup[aStateChoice]
#   if (is.null(countyChoices)) {
#     result <- aTibble %>%
#       filter(Province_State == selectedState)
#   } else {
#     result <- aTibble %>%
#       filter(stri_detect_fixed(Combined_Key, {{selectedState}}))
#   }
#   return(result)
# }

# dataForNewCasePlotsD <- function(forBoxplots, countyChoices, movingAvg, stateChoices) {
#   if ((!forBoxplots) && is.null(stateChoices)) {
#     if (movingAvg) {
#       theData <- US_Confirmed_Per100K_NewAvg
#     } else {
#       theData <- US_Confirmed_Per100K_New
#     }
#   } else {
#     if (is.null(countyChoices)) {
#       if (movingAvg) {
#         theData <- US_State_Confirmed_Per100K_NewAvg
#       } else {
#         theData <- US_State_Confirmed_Per100K_New
#       }
#     } else {
#       if (movingAvg) {
#         dataTibble <- US_County_Confirmed_Per100K_NewAvg
#       } else {
#         dataTibble <- US_County_Confirmed_Per100K_New
#       }
#       theData <- filterToStateChoiceD(dataTibble, stateChoices[1], countyChoices)
#     }
#   }
#   theData
# }

# plotNewCaseBoxplotsD <- function(chooseCounty,
#                                 movingAvg,
#                                 countyChoices,
#                                 stateChoices,
#                                 timeWindow) {
#   if (is.null(stateChoices)) {
#     theData <- dataForNewCasePlotsD(TRUE, NULL, movingAvg, stateChoices)
#   } else {
#     theData <- dataForNewCasePlotsD(TRUE, countyChoices, movingAvg, stateChoices)
#   }
#   
#   assembleDirectBoxPlot(theData, chooseCounty,
#                         countyChoices, stateChoices,
#                         newCasePlotTitle(TRUE, is.null(stateChoices), movingAvg,
#                                          is.null(countyChoices), stateChoices[1]),
#                         timeWindowXLabel(timeWindow),
#                         newCaseYLabel(),
#                         clampFactor = 3, timeWindow = timeWindow)
# }

# plotNewCaseTrendD <- function(chooseCounty,
#                              movingAvg,
#                              countyChoices,
#                              stateChoices,
#                              timeWindow) {
#   theData <- dataForNewCasePlotsD(FALSE, countyChoices, movingAvg, stateChoices)
#   
#   assembleDirectTrendPlot(theData, chooseCounty,
#                           countyChoices, stateChoices,
#                           newCasePlotTitle(FALSE, is.null(stateChoices), movingAvg,
#                                            is.null(countyChoices), stateChoices[1]),
#                           timeWindowXLabel(timeWindow),
#                           newCaseYLabel(),
#                           timeWindow)
# }

# chooseCounty <- FALSE
# movingAvg <- FALSE
# countyChoicesD <- c("Barnstable", "Dukes", "Suffolk", "Worcester")
# stateChoicesD <- c("MA", "ME")
# timeWindow <- 14

# foo78 <- filterToStateChoiceD(US_County_Confirmed_Per100K_NewAvg,
#                              stateChoicesD[1],
#                              NULL)

# p1 <- plotNewCaseBoxplotsD(chooseCounty, movingAvg,
#                            countyChoicesD, stateChoicesD,
#                            timeWindow)
# 
# print(p1$thePlot)

selectPlotDataD <- function(selectorRoutine, chooseCounty,
                           forBoxplot, countyChoices, movingAvg, stateChoices) {
  cat(file = stderr(), "Enter selectPlotDataD\n")
  cat(file = stderr(), "  chooseCounty =", chooseCounty, "\n")
  cat(file = stderr(), "  forBoxplot = ", forBoxplot, "\n")
  cat(file = stderr(), "  countyChoices = ", paste(countyChoices), "\n")
  cat(file = stderr(), "  movingAvg = ", movingAvg, "\n")
  cat(file = stderr(), "  stateChoices = ", paste(stateChoices), "\n")
  
  if (!chooseCounty) {
    cat(file = stderr(), "  setting countyChoices NULL\n")
    countyChoices <- NULL
  }
  if (is.null(stateChoices)) {
    cat(file = stderr(), "  selectorRoutine with NULL\n")
    theData <- selectorRoutine(forBoxplot, NULL, movingAvg, stateChoices)
  } else {
    cat(file = stderr(), "  selectorRoutine with countyChoices\n")
    theData <- selectorRoutine(forBoxplot, countyChoices, movingAvg, stateChoices)
  }
  cat(file = stderr(), "Leave selectPlotDataD\n")
  return(theData)
}

chooseCounty <- FALSE
movingAvg <- FALSE
countyChoicesD <- c("Barnstable", "Dukes", "Suffolk", "Worcester")
stateChoicesD <- c("MA", "ME")

foo <- selectPlotDataD(dataForTotalCasePlots, chooseCounty,
                       TRUE, countyChoicesD, movingAvg, stateChoicesD)


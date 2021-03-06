library(ggplot2)
library(tidyverse)
library(lubridate)

source("state_abbrev_lookup.R")
source("dateFormatRoutines.R")
source("computeNewAndGrowth.R")
source("columnUtilities.R")

leastSquaresTrendParams <- function(x0, y0)  {
  n = length(x0); n2 = length(y0)
  if (n == n2) {
    validData <- (!is.na(y0)) & (!is.nan(y0)) & (!is.infinite(y0))
    x <- x0[validData]
    y <- y0[validData]

    xsq <- x * x
    xy  <- x * y
    
    sumx <- sum(x)
    sumy <- sum(y)
    sumxsq <- sum(xsq)
    sumxy <- sum(xy)
    
    slope <- ( n * sumxy - sumx * sumy ) / ( n * sumxsq - sumx * sumx )
    intcp <- (sumy - slope * sumx ) / n
  } else {
    slope <- 0
    intcp <- sum(y)/n2
  }
  list(slope = slope, intercept = intcp)
}

processAreaChoices <- function(chooseCounty, countyChoices, stateChoices) {
  if (length(stateChoices) == 0) {
    areasOfInterest <- c("US")
  } else if (chooseCounty & (length(countyChoices) > 0)) {
    if (length(countyChoices) > 6) {
      countyChoices <- countyChoices[1:6]
    }
    sort(countyChoices)
    theState <- unname(stateLookup[stateChoices[1]])
    areasOfInterest <- paste(countyChoices, ", ", theState, ", US", sep="")
  } else {
    if (length(stateChoices) > 6) {
      stateChoices <- stateChoices[1:6]
    }
    sort(stateChoices)
    areasOfInterest <- paste(unname(stateLookup[stateChoices]), ", US", sep="")
  }
  areasOfInterest
}

processStateChoices <- function(stateChoices) {
  # Get a list of row names (to match Combined_Key entry) from abbreviations;
  #   whole US is default if no states are chosen
  ll <- length(stateChoices)
  if (length(stateChoices) == 0) {
    areasOfInterest <- c("US")
    stateChoices <- c("US")
    ll <- 1
  } else {
    if (length(stateChoices) > 6) {
      stateChoices <- stateChoices[1:6]
    }
    sort(stateChoices)
    areasOfInterest <- paste(unname(stateLookup[stateChoices]), ", US", sep="")
  }
  
  areasOfInterest  
}

computePlotDataFromFrame <- function(aFrame,
                                     chooseCounty,
                                     countyChoices,
                                     stateChoices,
                                     traceThisRoutine = FALSE,
                                     prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered computePlotDataFromFrame\n")
  }

  # "US" is default if stateChoices came in as Null,
  #    list is clamped to 6 states max

  areasOfInterest <- processAreaChoices(chooseCounty,
                                        countyChoices,
                                        stateChoices)
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "processAreaChoices returns", paste(areasOfInterest), "\n")
  }
  
  # Compute trend line data for selected states
  trendFrame <- aFrame
  
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "trendFrame names[1:5] = ", paste(names(trendFrame)[1:5]), "\n")
    cat(file = stderr(), myPrepend, "dim(trendFrame) = (", paste(dim(aFrame)), ")\n")
  }
  
  if (dim(trendFrame)[1] < 1) {
    return(list(plotData = NA, dateLabels = NA,
                AreasOfInterest = areasOfInterest))
  }

  for (i in 1:dim(trendFrame)[1]) {
    if (trendFrame[i, "Combined_Key"] %in% areasOfInterest) {
      aVec <- as_vector(select(aFrame, 2:last_col())[i,])
      attributes(aVec)$names <- NULL
      pars <- leastSquaresTrendParams(2:(length(aVec)+1), aVec)
      if (traceThisRoutine) {
        cat(file = stderr(), myPrepend, "i = ", i,
            "pars$slope = ", pars$slope,
            "pars$intercept", pars$intercept, "\n")
      }
      trendFrame[i,2:(length(aVec)+1)] <- as.list(pars$slope * 2:(length(aVec)+1) + pars$intercept)
    } else {
      trendFrame[i,2:(dim(aFrame)[2] - 1)] <- NA
    }
  }
  
  # Put together a data frame here and return it
  # Build sortable date strings for dateCol of plot data

  firstDate <- mdy(names(trendFrame)[2])
  
  prevDate   <- firstDate - 1
  dateCol    <- NULL
  indexCol   <- NULL
  yCol       <- NULL
  trendCol   <- NULL
  dateLabels <- NULL
  
  for (i in 2:dim(trendFrame)[2]) {
    thatDate      <- prevDate + i - 1
    aSortableDate <- format(thatDate, "%y/%m/%d")
    dateCol       <- c(dateCol,  rep(aSortableDate, dim(trendFrame)[1]))
    aDateLabel    <- str_replace(format(thatDate, "%b %d"), " 0", " ")
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend,
          "aSortableDate =", aSortableDate,
          "aDateLabel =", aDateLabel, "\n")
    }
    dateLabels    <- c(dateLabels, aDateLabel)
    indexCol      <- c(indexCol, rep(i, dim(trendFrame)[1]))
    forYCol       <- as_vector(aFrame[,i])
    forTrendCol   <- as_vector(trendFrame[,i])
    attributes(forYCol)$names <- NULL
    attributes(forTrendCol)$names <- NULL
    yCol       <- c(yCol,    forYCol)
    trendCol   <- c(trendCol, forTrendCol)
  }

  stateColOnce <- as_vector(trendFrame[,"Combined_Key"])
  attributes(stateColOnce)$names <- NULL
  
  stateCol <- rep(stateColOnce, dim(trendFrame)[2] - 1)

  plotData <- data.frame(date = dateCol, index = indexCol,
                         nums = yCol, trends = trendCol, region = stateCol)

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "areasOfInterest = ", paste(areasOfInterest), "\n")
    cat(file = stderr(), prepend, "Leaving computePlotDataFromFrame\n")
  }

  list(plotData = plotData, dateLabels = dateLabels, AreasOfInterest = areasOfInterest)
}

computePlotDataDirectFromCumulative <- function(aFrame, chooseCounty,
                                                countyChoices, stateChoices,
                                                timeWindow,
                                                tibbleName = "from computePlotDataDirectFromCumulative",
                                                traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered computePlotDataDirectFromCumulative\n")
    cat(file = stderr(), myPrepend, "dim(aFrame) = (", paste(dim(aFrame)), ")\n")
  }

  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "stateChoices = ", paste(stateChoices), "\n")
  }

  result = computePlotDataFromFrame(computeNewOnDayAndGrowthRate(aFrame,
                                                                 today("EST"),
                                                                 timeWindow,
                                                                 tibbleName = tibbleName,
                                                                 traceThisRoutine = traceThisRoutine,
                                                                 prepend = myPrepend)$d2,
                                    chooseCounty,
                                    countyChoices,
                                    stateChoices, traceThisRoutine = traceThisRoutine,
                                    prepend = myPrepend)
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "result$AreasOfInterest = ", paste(result$AreasOfInterest), "\n")
    cat(file = stderr(), prepend, "Leaving computePlotDataDirectFromCumulative\n")
  }

  return(result)
}

computeEvenZeroPlotDataDirect <- function(aFrame, chooseCounty,
                                          countyChoices, stateChoices,
                                          timeWindow,
                                          tibbleName = "<?>",
                                          traceThisRoutine = FALSE, prepend = "") {
  myPrepend  <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered computeEvenZeroPlotDataDirect\n")
  }

  result <- computePlotDataFromFrame(computeNewOnDayAndGrowthRate(aFrame,
                                                                  today("EST"),
                                                                  timeWindow,
                                                                  getGrowthRate = FALSE,
                                                                  nonzeroOnly = FALSE,
                                                                  tibbleName = tibbleName,
                                                                  traceThisRoutine = traceThisRoutine,
                                                                  prepend = myPrepend)$d2,
                                     chooseCounty,
                                     countyChoices,
                                     stateChoices)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving computeEvenZeroPlotDataDirect\n")
  }

  return(result)
}

computeGrowthPlotDataFromCumulative <- function(aFrame, chooseCounty,
                                                countyChoices, stateChoices,
                                                timeWindow,
                                                tibbleName = "<?>",
                                                traceThisRoutine = FALSE, prepend = "") {
  myPrepend  <- paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered computeGrowthPlotDataFromCumulative\n")
  }
  
  ofInterest <- stateChoices
  
  newAndGrowthList <- computeNewOnDayAndGrowthRate(aFrame,
                                                   today("EST"),
                                                   timeWindow,
                                                   tibbleName = tibbleName,
                                                   traceThisRoutine = traceThisRoutine,
                                                   prepend = myPrepend)
  res <- computePlotDataFromFrame(newAndGrowthList$growth,
                                  chooseCounty,
                                  countyChoices,
                                  ofInterest)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered computeGrowthPlotDataFromCumulative\n")
  }
  
  return(res)
}

clampDataIfRequested <- function(plotData, clampFactor,
                                 traceThisRoutine = FALSE, prepend = "") {
  myPrepend  <- paste("  ", prepend)
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered clampDataIfRequested\n")
  }

  # boxplot.stats gives the upper bound for non-outliers and a
  #  vector of outliers (among other things) so we can clamp the
  #  outliers to a reasonable scale
  bps <- boxplot.stats(plotData$nums, coef = 1.5 * clampFactor)

  # Pass bps back to the caller so we can display a list of
  # where outliers come from, if we so choose.
  
  # Any data values > bps$stats[5] are outliers.
  # Two calls to mutate() will clamp them within a scale which
  # allows study of the bulk of the data.
  if (traceThisRoutine) {
    cat(file = stderr(), myPrepend, "clampFactor is ", format(clampFactor, digits=3), "\n")
    cat(file = stderr(), myPrepend, "Boxplot outlier threshold is", format(bps$stats[5], digits=3), "\n")
  }
  
  newData <- plotData %>%
    mutate(num1 = if_else(nums > 0, nums, 0),
           .keep="unused") %>%
    mutate(nums = if_else(num1 < bps$stats[5], num1, bps$stats[5]),
           .keep="unused")
  
  clampedN <-  length(bps$out)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving clampDataIfRequested\n")
  }

  list(plotData=newData, diags=list(bps=bps, clampedN=clampedN))
}

assembleSomeBoxPlot <- function(res, theTitle, xlabel, ylabel,
                                clampFactor = 3,
                                traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleSomeBoxPlot\n")
  }

  if (!is.na(res["plotData"])) {
    finitizedPlotData <- res$plotData %>%
      mutate(nums = ifelse(is.na(nums), 50., nums), 
             .keep="unused",
             .after=index) %>%
      mutate(nums = ifelse((nums==-Inf), -100, nums),
             .keep="unused",
             .after=index)
  
    myBreaks <- unique(res$plotData$date)
    dateLabels <- res$dateLabels
  
    clampedList <- clampDataIfRequested(finitizedPlotData, clampFactor)
    plotData    <- clampedList$plotData
  
    palette("default")
  
    p <- ggplot(data = plotData)
    p <- p + geom_boxplot(mapping = aes(x = date, y = nums), na.rm = TRUE)
    p <- p + geom_point(data = filter(plotData, region %in% res$AreasOfInterest),
                        mapping = aes(x = date, y = nums, color = region), na.rm = TRUE)
    p <- p + labs(title = theTitle)
    p <- p + xlab(xlabel)
    p <- p + ylab(ylabel)
    p <- p + scale_x_discrete(breaks = myBreaks,
                              labels = dateLabels)
    result <- list(thePlot = p,
                   diags   = list(theData      = plotData,
                                  clampedN     = clampedList$clampedN,
                                  boxplotStats = clampedList$bps))
  } else {
    result <- list(thePlot = NA,
                   diags   = list(theData      = NA,
                                  clampedN     = NA,
                                  boxplotStats = NA))
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving assembleSomeBoxPlot\n")
  }

  return(result)
}
 
assembleDirectBoxPlot <- function(aFrame, chooseCounty,
                                  countyChoices, stateChoices,
                                  theTitle, xlabel, ylabel,
                                  clampFactor = 3, timeWindow = 14,
                                  tibbleName = "from assembleDirectBoxPlot",
                                  traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleDirectBoxPlot\n")
    cat(file = stderr(), myPrepend, "dim(aFrame) = (", paste(dim(aFrame)), ")\n")
  }
  res <- computePlotDataDirectFromCumulative(aFrame, chooseCounty,
                                             countyChoices, stateChoices,
                                             timeWindow, tibbleName = tibbleName,
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)
  result <- assembleSomeBoxPlot(res, theTitle, xlabel, ylabel, clampFactor,
                                traceThisRoutine = traceThisRoutine,
                                prepend = myPrepend)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving assembleDirectBoxPlot\n")
  }

  return(result)
}

assembleDirectBoxPlot_B <- function(aFrame, chooseCounty,
                                  countyChoices, stateChoices,
                                  theTitle, xlabel, ylabel,
                                  clampFactor = 3, timeWindow = 14,
                                  tibbleName = "from assembleDirectBoxPlot_B",
                                  traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleDirectBoxPlot_B\n")
    cat(file = stderr(), myPrepend, "dim(aFrame) = (", paste(dim(aFrame)), ")\n")
  }
  
  newData <- selectDataNDaysToDate(aFrame,
                                   today("EST"),
                                   nDays = timeWindow)
  dataToPlot = computePlotDataFromFrame(newData,
                                        chooseCounty,
                                        countyChoices,
                                        stateChoices,
                                        traceThisRoutine = traceThisRoutine,
                                        prepend = myPrepend)

  result <- assembleSomeBoxPlot(dataToPlot, theTitle, xlabel, ylabel, clampFactor,
                                traceThisRoutine = traceThisRoutine,
                                prepend = myPrepend)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving assembleDirectBoxPlot_B\n")
  }
  
  return(result)
}

assembleGrowthBoxPlot <- function(aFrame, chooseCounty,
                                  countyChoices, stateChoices,
                                  theTitle, xlabel, ylabel,
                                  clampFactor = 3, timeWindow = 14,
                                  tibbleName = "<?>",
                                  traceThisRoutine = FALSE,
                                  prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleGrowthBoxPlot\n")
    cat(file = stderr(), myPrepend, "dim(aFrame) = (", paste(dim(aFrame)), ")\n")
  }
  res <- computeGrowthPlotDataFromCumulative(aFrame, chooseCounty,
                                             countyChoices, stateChoices,
                                             timeWindow,
                                             tibbleName = tibbleName,
                                             traceThisRoutine = traceThisRoutine, prepend = myPrepend)
  
  if (is.na(res)["plotData"]) {
    result <- NA
    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "assembleGrowthBoxPlot returning NA\n")
    }
  } else {
    result <- assembleSomeBoxPlot(res, theTitle, xlabel, ylabel, clampFactor,
                                  traceThisRoutine = traceThisRoutine,
                                  prepend = myPrepend)
  }
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving assembleGrowthBoxPlot\n")
  }
  # OUCH caller must defend against returned NA ( if(is.na(<return value>)) {} )
  return(result)
}

assembleSomeTrendPlot <- function(res, theTitle, xlabel, ylabel,
                                  traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleSomeTrendPlot\n")
  }
  if (!is.na(res)["plotData"]) {
    dateLabels <- res$dateLabels
    
    palette("default")
    
    p <- ggplot(data = res$plotData)
    
    p <- p + geom_point(data = filter(res$plotData, region %in% res$AreasOfInterest),
                        mapping = aes(x = (index - 1), y = nums, color = region), na.rm = TRUE)
    p <- p + geom_line(data = filter(res$plotData, region %in% res$AreasOfInterest),
                       mapping = aes(x = (index - 1), y = trends, color = region), na.rm = TRUE)
    p <- p + labs(title = theTitle)
    p <- p + xlab(xlabel)
    p <- p + ylab(ylabel)
    p <- p + scale_x_continuous(breaks = 1:length(res$dateLabels),
                                labels = res$dateLabels,
                                minor_breaks = NULL)
  } else {
    p <- NA
  }

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving assembleSomeTrendPlot\n")
  }
  
  return(p)
}

assembleDirectTrendPlot <- function(aFrame, chooseCounty,
                                    countyChoices, stateChoices,
                                    theTitle, xlabel, ylabel,
                                    timeWindow = 14,
                                    tibbleName = "from assembleDirectTrendPlot",
                                    traceThisRoutine = FALSE,
                                    prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleDirectTrendPlot\n")
  }

  res <- computePlotDataDirectFromCumulative(aFrame, chooseCounty,
                                             countyChoices, stateChoices,
                                             timeWindow,
                                             tibbleName = tibbleName,
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)
  p <- assembleSomeTrendPlot(res, theTitle, xlabel, ylabel,
                             traceThisRoutine = traceThisRoutine,
                             prepend = myPrepend)
  result <- list(thePlot = p,
                 diags   = list(theData      = res$plotData,
                                clampedN     = NA,
                                boxplotStats = NA))

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving assembleDirectTrendPlot\n")
  }
  return(result)
}

assembleGrowthTrendPlot <- function(aFrame, chooseCounty,
                                    countyChoices, stateChoices,
                                    theTitle, xlabel, ylabel,
                                    timeWindow = 14,
                                    tibbleName = "from assembleGrowthTrendPlot",
                                    traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste(prepend, "  ", sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleGrowthTrendPlot\n")
  }

  res <- computeGrowthPlotDataFromCumulative(aFrame, chooseCounty,
                                             countyChoices, stateChoices,
                                             timeWindow,
                                             tibbleName = tibbleName)
  
  if (is.na(res)["plotData"]) {
    p <- NA

    if (traceThisRoutine) {
      cat(file = stderr(), myPrepend, "assembleGrowthTrendPlot returning NA\n")
    }
  } else {
    if (dim(filter(res$plotData, region %in% res$AreasOfInterest))[1] <= 0) {
      res <- computeEvenZeroPlotDataDirect(aFrame, chooseCounty,
                                           countyChoices, stateChoices,
                                           timeWindow)

      theTitle <- str_replace(theTitle, "Growth", "Number")
      # we have:      title <- "COVID Case Growth Trends for Selected States"
      # must change "Trends" to "Numbers"
      
      # "Daily growth rate: new day's deaths as percent of previous total deaths",
      
      # and ylabels "Daily growth rate: new day's cases as percent of previous total cases",
      # change to   "Daily growth: new day's cases"
      ylabel <- str_replace(ylabel, "growth rate", "growth")
      ylabel <- str_replace(ylabel, "as percent of previous total cases", "")
      ylabel <- str_replace(ylabel, "as percent of previous total deaths", "")
    }
    p <- assembleSomeTrendPlot(res, theTitle, xlabel, ylabel)
  }    
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving assembleGrowthTrendPlot\n")
  }
  
  list(thePlot = p,
       diags   = list(theData      = res$plotData,
                      clampedN     = NA,
                      boxplotStats = NA))
}

ratioFrame <- function(numeratorFrame, denominatorFrame,
                       timeWindow,
                       numTibbleName = "<?>",
                       denomTibbleName = "<?>",
                       traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  # traceThisRoutine <- TRUE
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered ratioFrame\n")
    cat(file = stderr(), myPrepend, "Numerator", numTibbleName, "\n")
    cat(file = stderr(), myPrepend, "Denominator", denomTibbleName, "\n")
    dD <- dim(denominatorFrame)
    cat(file = stderr(), myPrepend,
        "Dim denominatorFrame =", dD[1], dD[2], "\n")
  }
  
  # Filter zeros out of denominator
  denominatorGoodData <- denominatorFrame %>%
    select(Combined_Key, matches("^[0-9]+"))
  
  theColNames <- names(denominatorGoodData)
  for (i in 2:dim(denominatorGoodData)[2]) {
    theColName <- theColNames[i]
    denominatorGoodData <- denominatorGoodData %>%
      filter(.data[[theColName]] > 0)
  }

  denominatorNZKeys <- denominatorGoodData %>%
    select(Combined_Key)

  numeratorUsable <- left_join(denominatorNZKeys,
                               numeratorFrame %>%
                                 select(Combined_Key, matches("^[0-9]+")),
                               by = "Combined_Key")
  
  numeratorData <- select(numeratorUsable, matches("^[0-9]+"))
  denominatorData <- select(denominatorGoodData, matches("^[0-9]+"))
  
  # Do the division:
  if (traceThisRoutine) {
    dN <- dim(numeratorData)
    dD <- dim(denominatorData)
    if ((dN[1] == dD[1]) && (dN[2] == dD[2]) &&
        (names(numeratorData)[2] == names(denominatorData)[2]))
    {
      cat(file = stderr(), myPrepend,
          "Dim denominatorData =", dD[1], dD[2], "\n")
      cat(file = stderr(), myPrepend, "dims & col name[2]s match\n")
    } else {
      cat(file = stderr(), myPrepend,
          "Dim numeratorData =", dN[1], dN[2], "\n")
      cat(file = stderr(), myPrepend,
          "NumeratorData name[2] =", names(numeratorData)[2], "\n")
      cat(file = stderr(), myPrepend,
          "Dim denominatorData =", dD[1], dD[2], "\n")
      cat(file = stderr(), myPrepend,
          "DenominatorData name[2] =", names(denominatorData)[2], "\n")
    }
  }  
  pctPosTests <- as_tibble((100.0 * numeratorData) / denominatorData)
  
  # Put the "Combined_Key" column back in front (bind_cols)
  result <- bind_cols(denominatorNZKeys, pctPosTests)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving ratioFrame\n")
  }
  
  return(result)
}

ratioDeltaFrame <- function(numeratorFrame, denominatorFrame,
                            timeWindow,
                            numTibbleName = "<?>",
                            denomTibbleName = "<?>",
                            traceThisRoutine = FALSE, prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered ratioDeltaFrame")
  }
  # Ratio of changes, e.g. positive test rate <- (new cases today) / (people tested today) ==
  #    (growth in cases, today) / (growth in people tested, today)
  denominatorNew <- computeNewOnDayAndGrowthRate(denominatorFrame, today("EST"),
                                                 timeWindow,
                                                 FALSE, TRUE,
                                                 tibbleName = denomTibbleName,
                                                 traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend)$new
  numeratorNew   <- computeNewOnDayAndGrowthRate(numeratorFrame, today("EST"),
                                                 timeWindow,
                                                 FALSE, FALSE,
                                                 tibbleName = numTibbleName,
                                                 traceThisRoutine = traceThisRoutine,
                                                 prepend = myPrepend)$new
  
  keyTibND <- select(numeratorNew,   Combined_Key)
  keyTibDD <- select(denominatorNew, Combined_Key)
  keyTib   <- inner_join(keyTibND, keyTibDD, by = "Combined_Key")
  
  denominatorUsable <- left_join(keyTib, denominatorNew, by="Combined_Key")
  numeratorUsable   <- left_join(keyTib, numeratorNew, by="Combined_Key")
  
  dimND <- dim(numeratorUsable)
  dimDD <- dim(denominatorUsable)
  newPosTests      <- numeratorUsable[, 2:dimND[2]]
  newRelevantTests <- denominatorUsable[, 2:dimDD[2]]

  # Do the division:
  pctPosTests <- as_tibble((100.0 * newPosTests) / newRelevantTests)

  # Put the "Combined_Key" column back in front (bind_cols)
  result <- bind_cols(keyTib, pctPosTests)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving ratioDeltaFrame")
  }

  return(result)
}

# OUCH needs chooseCounty, countyChoices
assembleRatioDeltaBoxPlot <- function(numeratorFrame, denominatorFrame,
                                      stateChoices, theTitle, xlabel, ylabel,
                                      clampFactor = 3,
                                      timeWindow = 14,
                                      numTibbleName = "<?>",
                                      denomTibbleName = "<??>",
                                      traceThisRoutine = FALSE,
                                      prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleRatioDeltaBoxPlot\n")
  }

  myDataFrame <- ratioDeltaFrame(numeratorFrame, denominatorFrame, timeWindow,
                                 numTibbleName = numTibbleName,
                                 denomTibbleName = denomTibbleName,
                                 traceThisRoutine = traceThisRoutine,
                                 prepend = myPrepend)
  res <- computePlotDataDirectFromCumulative(myDataFrame, 
                                             FALSE, # OUCH chooseCounty,
                                             NULL,  # OUCH countyChoices,
                                             stateChoices, timeWindow,
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)

  myBreaks <- unique(res$plotData$date)
  dateLabels <- res$dateLabels

  clampedList <- clampDataIfRequested(res$plotData, clampFactor)
  plotData    <- clampedList$plotData
  
  palette("default")
  
  p <- ggplot(data = plotData)
  p <- p + geom_boxplot(mapping = aes(x = date, y = nums), na.rm = TRUE)
  p <- p + geom_point(data = filter(plotData, region %in% res$AreasOfInterest),
                      mapping = aes(x = date, y = nums, color = region), na.rm = TRUE)
  p <- p + labs(title = theTitle)
  p <- p + xlab(xlabel)
  p <- p + ylab(ylabel)
  p <- p + scale_x_discrete(breaks = myBreaks,
                            labels = dateLabels)
  
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving assembleRatioDeltaBoxPlot\n")
  }

  list(thePlot = p,
       diags   = list(theData      = plotData,
                      clampedN     = clampedList$clampedN,
                      boxplotStats = clampedList$bps))
}

# OUCH needs chooseCounty, countyChoices
assembleRatioDeltaTrendPlot <- function(numeratorFrame, denominatorFrame,
                                        stateChoices, theTitle, xlabel, ylabel,
                                        timeWindow = 14,
                                        numTibbleName = "<?>",
                                        denomTibbleName = "<??>",
                                        traceThisRoutine = FALSE,
                                        prepend = "") {
  myPrepend <- paste("  ", prepend, sep = "")
  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Entered assembleRatioDeltaTrendPlot\n")
  }

  myDataFrame <- ratioDeltaFrame(numeratorFrame, denominatorFrame, timeWindow,
                                 numTibbleName = numTibbleName,
                                 denomTibbleName = denomTibbleName,
                                 traceThisRoutine = traceThisRoutine,
                                 prepend = myPrepend)
  res <- computePlotDataDirectFromCumulative(myDataFrame,
                                             FALSE, # OUCH chooseCounty,
                                             NULL,  # OUCH countyChoices,
                                             stateChoices, timeWindow,
                                             traceThisRoutine = traceThisRoutine,
                                             prepend = myPrepend)
  
  dateLabels <- res$dateLabels
  dateLabels <- res$plotData$date
  
  palette("default")
  
  p <- ggplot(data = res$plotData)
  
  p <- p + geom_point(data = filter(res$plotData, region %in% res$AreasOfInterest),
                      mapping = aes(x = (index - 1), y = nums, color = region), na.rm = TRUE)
  p <- p + geom_line(data = filter(res$plotData, region %in% res$AreasOfInterest),
                     mapping = aes(x = (index - 1), y = trends, color = region))
  p <- p + labs(title = theTitle)
  p <- p + xlab(xlabel)
  p <- p + ylab(ylabel)
  p <- p + scale_x_continuous(breaks = 1:length(unique(dateLabels)),
                              labels = res$dateLabels, minor_breaks = NULL)

  result <- list(theData = res$plotData, thePlot = p)

  if (traceThisRoutine) {
    cat(file = stderr(), prepend, "Leaving assembleRatioDeltaTrendPlot\n")
  }

  return(result)
}


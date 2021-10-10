
boxplotHeaderHTML <- function(countyChoices, stateChoices) {
  if ((!is.na(countyChoices)) && (length(countyChoices) > 0)) {
    admin1Ts <- admin1TypeFor(stateChoices[1])$LC_PL
    middle_blank_1 <- paste(admin1Ts, " in",
                            stateLookup[stateChoices[1]])
    middle_blank_2 <- admin1Ts
  } else {
    middle_blank_1 <- "states"
    middle_blank_2 <- "states"
  }
  
  HTML(paste(tags$p(),
             tags$p(paste(
               "How to interpret these charts: The bar across the middle",
               " of the box gives the median of all ",
               middle_blank_1,
               " on that date. Half the ",
               middle_blank_2,
               " fall within the box, a quarter above it and",
               " a quarter below it. Dots in a horizontal line near",
               " the top of the graph are not real data, but mean there",
               " is a data point at that level or higher (if the graph",
               " were scaled to show everything, the interesting part",
               " of the graph would be all squished at the bottom)", sep = "")),
             tags$p(), tags$p()))
}

movingAvgWhy <- function() {
  paste(tags$p("A 7-day moving average will show trends more clearly."),
                      tags$p("Without averaging, states which do not report on Sundays, e.g.,
                    make the data look less uniform than it is."),
                      sep = "")
}

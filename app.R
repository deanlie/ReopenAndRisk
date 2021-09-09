#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)

source("mostRecentDataDate.R")
# source("updateTimeSeriesDataFilesAsNecessary.R")
# source("updateStateLevelSerializedDataFiles.R")
source("assemblePlotObject.R")
source("loadAllUSData.R")
source("latestVaccExtremes.R")

source("doCaseGrowthTab.R")
source("doMortalityTab.R")
source("doTestGrowthTab.R")
source("doSummaryTab.R")

# updateTimeSeriesDataFilesAsNecessary()
# updateStateLevelSerializedDataFilesAsNecessary()
loadAllUSData()

vaccHeaderHTML <- function(movingAvg, vaccChoice) {
  if (movingAvg) {
    tooMuchData <- US_State_Vaccination_Pcts_A7
  } else {
    tooMuchData <- US_State_Vaccination_Pcts
  }

  nMin <- 3
  nMax <- 3
  extremaStates <- latestVaccExtremes(tooMuchData, vaccChoice, nMin, nMax)
  
  theMaxPct <- format(as.double(extremaStates$bot[nMax, 2]), digits=3)
  max2Pct   <- format(as.double(extremaStates$bot[nMax - 1, 2]), digits=3)
  theMinPct <- format(as.double(extremaStates$top[1, 2]), digits=3)
  min2Pct   <- format(as.double(extremaStates$top[2, 2]), digits=3)
  theText <- paste(tags$h4(paste("Vaccinations,", vaccChoice)),
                   tags$p("Vaccination data is shown by percent of state or of US as a whole."),
                   tags$p(paste("Highest ", vaccChoice, " rate: ",
                                extremaStates$bot[nMax, 1],
                                " with ", theMaxPct,
                                " percent", sep = "")),
                   tags$p(paste("Next highest rate: ",
                                extremaStates$bot[nMax - 1, 1],
                                " with ", max2Pct,
                                " percent", sep = "")),
                   tags$p(paste("Lowest ", vaccChoice, " rate: ",
                                extremaStates$top[1, 1],
                                " with ", theMinPct,
                                " percent", sep = "")),
                   tags$p(paste("Next lowest rate: ",
                                extremaStates$top[2, 1],
                                " with ", min2Pct,
                                " percent", sep = "")),
                   tags$p("Note that 'Total Doses' will be above 100% when close to 50% of the population
                            has had a second dose!"),
                   sep="")
  
  HTML(theText)
}

vaccRBoxHTML <- function(movingAvg, vaccChoice) {
}

vaccRTrendHTML <- function(movingAvg, vaccChoice) {
}

testGrowthHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
    theText <- paste(tags$h4("Changes in Amount of Testing"),
                     tags$p("The data used for this tab is not updated as regularly as the
                            data for cases and mortality, and is not always reliable. Uneven updates
                            can result in numbers which change the scale so much that the resulting
                            chart is unreadable. To prevent that, these graphs do not display data
                            which is far outside the range of the bulk of the data. Dots along the
                            top or bottom of the chart are not real data."),
                     tags$p("The amount of testing should generally not be decreasing."),
                     tags$p("A downward sloping trend line is not necessarily a problem. It just means
                            that the amount of testing is not growing as fast as it used to be.
                            So long as the growth rate is above 0 the number of tests is increasing."),
                     sep="")
    HTML(theText)
}

testResultsHeaderHTML <- function(chooseCounty, countyChoices, stateChoices) {
    theText <- paste(tags$h4("Changes in Test Results"),
                     tags$p("A decreasing percentage of positive tests is good;
                            it means that tests are becoming more widely available."),
                     tags$p("Think of the extreme case: if everyone were tested,
                            and 0 percent of tests were positive, the epidemic would be over."),
                     sep="")
    HTML(theText)
}

plotVaccBoxplots <- function(movingAvg, vaccChoice, stateChoices, timeWindow) {
  plotTitleLookup <- c("First Doses"="First Vaccine Doses",
                       "Second Doses"="Second Vaccine Doses",
                       "Total Doses"="Total Vaccine Doses Administered",
                       "People Fully Vaccinated"="People Fully Vaccinated")
  title0 <- unname(plotTitleLookup[vaccChoice])
  if (movingAvg) {
    title <- paste(title0, "State Distribution, 7 day moving average")
    tooMuchData <- US_State_Vaccination_Pcts_A7
  } else {
    title <- paste(title0, "State Distribution")
    tooMuchData <- US_State_Vaccination_Pcts
  }

  theData <- makeFullyVaccDataIfNeeded(tooMuchData, vaccChoice)

  timeWindow <- min(timeWindow, dim(theData)[2] - 4)
  
  vaccTrendData <<- list(full=tooMuchData, filtered=theData)
  
  assembleDirectBoxPlot(theData, FALSE, NULL,
                        stateChoices,
                        title,
                        paste("Last", timeWindow, "days"),
                        "Vaccinations, percent of population",
                        clampFactor = 3, timeWindow = timeWindow)
}

plotVaccTrend <- function(movingAvg, vaccChoice, stateChoices, timeWindow) {
  if (movingAvg) {
    if (length(stateChoices) > 0) {
      title <- paste(vaccChoice, "For Selected States, 7 Day Moving Average")
      tooMuchData <- US_State_Vaccination_Pcts_A7
    } else {
      title <- paste(vaccChoice, ", ", "US Overall, 7 Day Moving Average", sep = "")
      tooMuchData <- US_Vaccination_Pcts_A7
    }
  } else {
    if (length(stateChoices) > 0) {
      title <- paste(vaccChoice, "For Selected States")
      tooMuchData <- US_State_Vaccination_Pcts
    } else {
      title <- paste(vaccChoice,", ", "US Overall", sep = "")
      tooMuchData <- US_Vaccination_Pcts
    }
  }

  theData <- makeFullyVaccDataIfNeeded(tooMuchData, vaccChoice)
  
  vaccTrendData <<- list(full=tooMuchData, filtered=theData)
  
  timeWindow = min(timeWindow, dim(theData)[2] - 4)
  
  assembleDirectTrendPlot(theData, FALSE,
                          NULL,
                          stateChoices,
                          title,
                          paste("Last", timeWindow, "days"),
                          "Vaccinations, percent of population",
                          timeWindow = timeWindow,
                          tibbleName = "from plotVaccTrend")
}

plotTestResultBoxplots <- function(chooseCounty, movingAvg, countyChoices,
                                   stateChoices, timeWindow) {
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
  
  assembleRatioDeltaBoxPlot(theCaseData, theTestData, stateChoices,
                            title,
                            paste("Last", timeWindow, "days"),
                            "Test Positivity: percent of tests returning positive",
                            clampFactor = 1,
                            timeWindow = timeWindow,
                            nFirstNum = 2, nFirstDenom = 2)
}

plotTestResultTrend <- function(chooseCounty, movingAvg, countyChoices,
                                stateChoices, timeWindow) {
  # updateDataForUSTypeIfNeeded("Confirmed")
  if (is.null(stateChoices)) {
    if (movingAvg) {
      title <- "COVID Test Positivity Trend for US as a whole, 7 day moving average"
      theCaseData <- US_Confirmed_A7
      theTestData <- US_People_Tested_A7
    } else {
      title <- "COVID Test Positivity Trend for US as a whole"
      theCaseData <- US_Confirmed
      theTestData <- US_People_Tested
    }
  } else {
    if (movingAvg) {
      title <- "COVID Test Positivity Trends for Selected States, 7 day moving average"
      theCaseData <- US_State_Confirmed_A7
      theTestData <- US_State_People_Tested_A7
    } else {
      title <- "COVID Test Positivity Trends for Selected States"
      theCaseData <- US_State_Confirmed
      theTestData <- US_State_People_Tested
    }
  }
    
  assembleRatioDeltaTrendPlot(theCaseData, theTestData, stateChoices,
                              title,
                              paste("Last", timeWindow, "days"),
                              "Test Positivity: percent of tests returning positive",
                              timeWindow = timeWindow,
                              nFirstNum = 2, nFirstDenom = 2)
}

# Define UI for this application
ui <- fluidPage(
    
    tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "rr_style1.css")),
    
    # Application title
    titlePanel("COVID-19 Risk Level Data"),
    
    # www/harvard_link.html includes citation:
    #    COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE)
    #      at Johns Hopkins University
    
    # Sidebar with a dropdown box for states, slider input for number of days 
    sidebarLayout(
        sidebarPanel(
            sliderInput("timeWindow",
                        "How many days to trace?",
                        min = 7,
                        max = 28,
                        value = 14,
                        step = 1),
            selectInput("stateChoices",
                        "Select up to six states",
                        names(stateLookup),
                        multiple = TRUE),
            tags$p("County data is available on the Case Growth, Mortality, and Summary tabs."),
            checkboxInput("chooseCounty", "Select up to six counties"), # "Select County(ies)"),
            # Without the spacer, the County dropbox overwrote the
            #  caption "Select County/Counties.
            tags$p("", id="Spacer"),
            checkboxInput("movingAvg", "Plot 7-day moving average", value = TRUE),
            tags$p("", id="Spacer"),
            tags$img(src="TeamScience_500.png",
                     width="100%"),
            tags$p("Thank you @jesch30 and @shineboltstation for the drawing"),
            tags$p("", id="Spacer"),
            tags$p("And thanks to Tampa R Ladies for encouragement and R workshops"),
            includeHTML("./www/from_cdc_stmt.html")
        ),
        
        # Show a plot of the generated distribution

        mainPanel(
            tabsetPanel(
                tabPanel("Vaccination Progress",
                         verticalLayout(
                           mainPanel(selectInput("Vaccination",
                                                 "Choose one item",
                                                 c("First Doses",
                                                   "Second Doses",
                                                   "Total Doses",
                                                   "People Fully Vaccinated"),
                                                 multiple = FALSE,
                                                 selected = "Total Doses"),
                                     htmlOutput("vaccHeaderHTML"),
                                     htmlOutput("vaccRboxHTML"),
                                     plotOutput("vaccRBox"),
                                     htmlOutput("vaccRTrendHTML"),
                                     plotOutput("vaccRTrend")))),
                tabPanel("Case Growth",
                         mainPanel(htmlOutput("caseHeaderHTML"),
                                   plotOutput("caseBox"),
                                   plotOutput("caseTrend"))),
                tabPanel("Mortality",
                         mainPanel(htmlOutput("mortalityHeaderHTML"),
                                   # tags$p(textOutput("mortalityP1")),
                                   # tags$p(textOutput("mortalityP2")),
                                   
                                   plotOutput("mortalityBox"),
                                   plotOutput("mortalityTrend"))),
                tabPanel("Test Growth",
                         mainPanel(htmlOutput("testGrowthHeaderHTML"),
                                   plotOutput("testGBox"),
                                   plotOutput("testGTrend"))),
                tabPanel("Test Results",
                         mainPanel(htmlOutput("testResultsHeaderHTML"),
                                   plotOutput("testRBox"),
                                   plotOutput("testRTrend"))),
                tabPanel("Summary",
                         includeHTML("./www/harvard_link.html"),
                         mainPanel(htmlOutput("summaryHTML"))))
            )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  countyChoices <- reactive({
    allChoices <- input$stateChoices
    theStateAbbrev <- allChoices[1]
    # print(paste("input State[1] is", theStateAbbrev))
    list(stAbv = theStateAbbrev,
         choices = CountiesByState$County[CountiesByState$State ==
                                            unname(stateLookup[allChoices[1]])])
  })
  
  useCounty <- reactive({input$chooseCounty})
  
  observeEvent(countyChoices(), {
    if(!input$chooseCounty) {
      removeUI(selector = "#CountyXX")
    } else {
      if(! is.na(countyChoices()$choices[1])) {
        admin1 <- admin1TypeFor(countyChoices()$stAbv)$UC_S
# 
#         admin1 <- "County"
#         if (length(countyChoices()$choices) > 0) {
#           if (! is.null(countyChoices()$stAbv)) {
#             if (countyChoices()$stAbv == "LA") {
#               admin1 <- "Parish"
#             }
#             if (countyChoices()$stAbv == "PR") {
#               admin1 <- "Municipio"
#             }
#           }
#         }
        if (length(countyChoices()$choices > 0)) {
          updateSelectInput(session, 'countyChoices',
                            label = admin1,
                            choices = countyChoices()$choices,
                            selected = NULL) # }
        }
      }
    }
  })
  
  observeEvent(useCounty(), {
    if ((length(input$stateChoices) > 0) && input$stateChoices[1] != "DC") {
      if (input$chooseCounty) {
        admin1 <- admin1TypeFor(countyChoices()$stAbv)$UC_S
        updateCheckboxInput(session, "movingAvg", value = TRUE)
        # admin1 <- "County"
        # if (length(input$stateChoices) > 0) {
        #   if (input$stateChoices[1] == "LA") {
        #     admin1 <- "Parish"
        #   }
        #   if (countyChoices()$stAbv == "PR") {
        #     admin1 <- "Municipio"
        #   }
        # }
        insertUI(selector = "#Spacer", where="afterEnd",
                 # I couldn't figure out how to to make a selector work (for the call
                 # to removeUI) directly on the UI component created by
                 # selectizeInput, so I wrapped it all in a div and selected
                 # on the div's ID.
                 tags$div(id = "CountyXX",
                          selectInput(inputId = "countyChoices",
                                      label = admin1,
                                      choices = countyChoices()$choices, multiple = TRUE),
                          tags$p("", id="Spacer2")))
        
      } else {
        removeUI(selector = "#CountyXX")
      }
    } else {
      removeUI(selector = "#CountyXX")
    }
  })

  # "Cases" Tab
  output$vaccHeaderHTML <- renderUI({vaccHeaderHTML(input$movingAvg,
                                                    input$Vaccination)})
  output$vaccRBoxHTML   <- renderUI({vaccRBoxHTML(input$movingAvg,
                                                  input$Vaccination)})
  output$vaccRTrendHTML <- renderUI({vaccRTrendHTML(input$movingAvg,
                                                    input$Vaccination)})    

  output$vaccRBox <- renderPlot({plotVaccBoxplots(input$movingAvg,
                                                  input$Vaccination,
                                                  input$stateChoices,
                                                  input$timeWindow)})
  output$vaccRTrend <- renderPlot({plotVaccTrend(input$movingAvg,
                                                    input$Vaccination,
                                                    input$stateChoices,
                                                    input$timeWindow)})

  output$caseHeaderHTML <- renderUI({caseHeaderHTML(input$chooseCounty,
                                                    input$countyChoices,
                                                    input$stateChoices)})
  output$caseBox <- renderPlot({plotCaseGrowthBoxplots(input$chooseCounty,
                                                       input$movingAvg,
                                                       input$countyChoices,
                                                       input$stateChoices,
                                                       input$timeWindow)})
  output$caseTrend <- renderPlot({plotCaseGrowthTrend(input$chooseCounty,
                                                      input$movingAvg,
                                                      input$countyChoices,
                                                      input$stateChoices,
                                                      input$timeWindow)})
  
  # "Mortality" tab    
  output$mortalityHeaderHTML <- renderUI({mortalityHeaderHTML(input$chooseCounty,
                                                              input$countyChoices,
                                                              input$stateChoices)})
  output$mortalityBox <- renderPlot({plotMortalityGrowthBoxplots(input$chooseCounty,
                                                                 input$movingAvg,
                                                                 input$countyChoices,
                                                                 input$stateChoices,
                                                                 input$timeWindow)})
  output$mortalityTrend <- renderPlot({plotMortalityGrowthTrend(input$chooseCounty,
                                                                input$movingAvg,
                                                                input$countyChoices,
                                                                input$stateChoices,
                                                                input$timeWindow)})
  
  # "Test Growth" tab
  output$testGrowthHeaderHTML <- renderUI({testGrowthHeaderHTML(input$chooseCounty,
                                                                input$countyChoices,
                                                                input$stateChoices)})
  output$testGBox <- renderPlot({plotTestGrowthBoxplots(input$chooseCounty,
                                                        input$movingAvg,
                                                        input$countyChoices,
                                                        input$stateChoices,
                                                        input$timeWindow)})
  output$testGTrend <- renderPlot({plotTestGrowthTrend(input$chooseCounty,
                                                       input$movingAvg,
                                                       input$countyChoices,
                                                       input$stateChoices,
                                                       input$timeWindow)})
  
  # "Test Results" tab
  output$testResultsHeaderHTML <- renderUI({testResultsHeaderHTML(input$chooseCounty,
                                                                  input$countyChoices,
                                                                  input$stateChoices)})
  output$testRBox <- renderPlot({plotTestResultBoxplots(input$chooseCounty,
                                                        input$movingAvg,
                                                        input$countyChoices,
                                                        input$stateChoices,
                                                        input$timeWindow)})
  output$testRTrend <- renderPlot({plotTestResultTrend(input$chooseCounty,
                                                       input$movingAvg,
                                                       input$countyChoices,
                                                       input$stateChoices,
                                                       input$timeWindow)})
  
  # "Summary" tab
  output$summaryHTML <- renderUI({
    summaryHTMLAny(input$chooseCounty,
                   input$countyChoices,
                   input$stateChoices)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

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
source("updateTimeSeriesDataFilesAsNecessary.R")
source("updateStateLevelSerializedDataFiles.R")
source("assemblePlotObject.R")
source("loadAllUSData.R")
source("reopenPlotUtilities.R")

source("doVaccinationTab.R")
source("doCaseGrowthTab.R")
source("doMortalityTab.R")
source("doTestGrowthTab.R")
source("doTestResultsTab.R")
source("doSummaryTab.R")

updateTimeSeriesDataFilesAsNecessary()
updateStateLevelSerializedDataFilesAsNecessary()
loadAllUSData()

currentlyTesting <- function() {
  TRUE
}

currentlyTestingCounties <- function() {
  currentlyTesting() & TRUE
}

defaultTimeWindowValue <- function() {
  if (currentlyTesting()) {
    14
  } else {
    14
  }
}

defaultStateChoices <- function() {
  if (currentlyTesting()) {
    c("MA", "ME")
  } else {
    NULL
  }
}

defaultSelectedTab <- function() {
  if (currentlyTesting()) {
    "Test Results"
  } else {
    NULL
  }
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
                        value = defaultTimeWindowValue(),
                        step = 1),
            selectInput("stateChoices",
                        "Select up to six states",
                        names(stateLookup),
                        selected = defaultStateChoices(),
                        multiple = TRUE),
            tags$p("County data is available on the New Cases, New Deaths, and Summary tabs."),
            checkboxInput("chooseCounty", "Select up to six counties",
                          value = currentlyTestingCounties()), # "Select County(ies)"),
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
                tabPanel("New Cases",
                         mainPanel(htmlOutput("caseHeaderHTML"),
                                   plotOutput("caseBox"),
                                   plotOutput("caseTrend"))),
                tabPanel("New Deaths",
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
                         mainPanel(htmlOutput("summaryHTML"))),
                selected = defaultSelectedTab())
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
        # Be sure we use "Parish" or "Municipio" instead of "County" for
        #  Louisiana, Puerto Rico respectively. Alaska is a problem also
        #  because it has counties and non-county administrative entities
        admin1 <- admin1TypeFor(countyChoices()$stAbv)$UC_S
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
        # cf comment above
        admin1 <- admin1TypeFor(countyChoices()$stAbv)$UC_S
        updateCheckboxInput(session, "movingAvg", value = TRUE)
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

  # "Vaccination" Tab
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

  # "Cases" Tab
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

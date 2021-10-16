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
library(gt)
library(tidyverse)
library(lubridate)

source("mostRecentDataDate.R")
source("assemblePlotObject.R")
source("loadAllUSData.R")
source("reopenPlotUtilities.R")
source("makeGtPresentation.R")
source("boxplotHeaderHTML.R")
source("doVaccinationTab.R")
source("doNewCaseTab.R")
source("doTotalCaseTab.R")
source("doNewDeathsTab.R")
source("doTotalDeathsTab.R")
source("doTestGrowthTab.R")
source("doTestResultsTab.R")
source("doSummaryTab.R")

manualStaticData <- function() {
  FALSE
}

manualTraceModeQ <- function() {
  FALSE
}

testModeQ <- function() {
  manualStaticData() || isTRUE(getOption("shiny.testmode"))
}

loadAllUSData(staticDataQ = testModeQ(),
              traceThisRoutine = manualTraceModeQ(),
              prepend = "")

defaultTimeWindowValue <- function() {
  if (testModeQ()) {
    7
  } else {
    14
  }
}

defaultSelectedTab <- function() {
  "Vaccination Progress"
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
                        selected = NULL,
                        multiple = TRUE),
            tags$p("County data is available on the New Cases, New Deaths, and Summary tabs."),
            checkboxInput("chooseCounty", "Select up to six counties",
                          value = FALSE), # "Select County(ies)"),
            # Without the spacer, the County dropbox overwrote the
            #  caption "Select County/Counties.
            tags$p("", id="Spacer"),
            checkboxInput("movingAvg", "Use 7-day moving average", value = TRUE),
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
          tabsetPanel(id = "mainTabsetPanel",
            tabPanel("Vaccination Progress",
              verticalLayout(
                mainPanel(selectInput("Vaccination",
                                      "Choose one item",
                                      c("First Doses",
                                        "Second Doses",
                                        "Total Doses",
                                        "People Fully Vaccinated"),
                                      multiple = FALSE,
                                      selected = "People Fully Vaccinated"),
                htmlOutput("vaccHeaderHTML"),
                tabsetPanel(id = "vaccinationTabsetPanel",
                  tabPanel("Boxplot",
                    verticalLayout(
                      htmlOutput("vaccRboxHTML"),
                      plotOutput("vaccRBox"))),
                  tabPanel("Trend Line",
                    verticalLayout(
                      htmlOutput("vaccRTrendHTML"),
                      plotOutput("vaccRTrend"))),
                  tabPanel("Data",
                    verticalLayout(
                      htmlOutput("vaccDataHTML"),
                      gt_output("vaccGtData")
                    )))))),
            tabPanel("New Cases",
              verticalLayout(
                htmlOutput("newCaseHeaderHTML"),
                tabsetPanel(id = "newCaseTabsetPanel",
                  tabPanel("Boxplot",
                    verticalLayout(
                      htmlOutput("newCaseBoxplotHeaderHTML"),
                      plotOutput("newCaseBox"))),
                  tabPanel("Trend Line",
                           plotOutput("newCaseTrend")),
                  tabPanel("Data",
                           gt_output("newCaseGtData"))))),
            tabPanel("Total Cases",
              verticalLayout(
                htmlOutput("totalCaseHeaderHTML"),
                tabsetPanel(id = "totalCaseTabsetPanel",
                  tabPanel("Boxplot",
                    verticalLayout(
                      htmlOutput("totalCaseBoxplotHeaderHTML"),
                      plotOutput("totalCaseBox"))),
                  tabPanel("Trend Line",
                           plotOutput("totalCaseTrend")),
                  tabPanel("Data",
                           verticalLayout(
                             gt_output("totalCaseGtData")
                           ))))),
            tabPanel("New Deaths",
              verticalLayout(
                htmlOutput("newDeathsHeaderHTML"),
                tabsetPanel(id = "newDeathsTabsetPanel",
                  tabPanel("Boxplot",
                           verticalLayout(
                             htmlOutput("newDeathsBoxplotHeaderHTML"),
                             plotOutput("newDeathsBox"))),
                  tabPanel("Trend Line",
                           plotOutput("newDeathsTrend")),
                  tabPanel("Data",
                           verticalLayout(
                             gt_output("newDeathsGtData")
                           ))))),
            tabPanel("Total Deaths",
              verticalLayout(
                htmlOutput("totalDeathsHeaderHTML"),
                tabsetPanel(id = "totalDeathsTabsetPanel",
                  tabPanel("Boxplot",
                           verticalLayout(
                             htmlOutput("totalDeathsBoxplotHeaderHTML"),
                             plotOutput("totalDeathsBox"))),
                  tabPanel("Trend Line",
                           plotOutput("totalDeathsTrend")),
                  tabPanel("Data",
                           verticalLayout(
                             # htmlOutput("totalDeathsDataHTML"),
                             gt_output("totalDeathsGtData")
                           ))))),
            tabPanel("Test Growth",
              verticalLayout(
                htmlOutput("testGrowthHeaderHTML"),
                tabsetPanel(id = "testGrowthTabsetPanel",
                  tabPanel("Boxplot",
                    verticalLayout(
                      htmlOutput("testGBoxplotHeaderHTML"),
                      plotOutput("testGBox"))),
                  tabPanel("Trend Line",
                           plotOutput("testGTrend")),
                  tabPanel("Data",
                           verticalLayout(
                             # htmlOutput("testGrowthDataHTML"),
                             gt_output("testGrowthGtData")
                           ))))),
            tabPanel("Test Results",
              verticalLayout(
                htmlOutput("testResultsHeaderHTML"),
                tabsetPanel(id = "testResultsTabsetPanel",
                  tabPanel("Boxplot",
                    verticalLayout(
                      htmlOutput("testRBoxplotHeaderHTML"),
                      plotOutput("testRBox"))),
                  tabPanel("Trend Line",
                           plotOutput("testRTrend")),
                  tabPanel("Data",
                           verticalLayout(
                             gt_output("testResultsGtData")
                           ))))),
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

  isTestModeQ <- testModeQ()

  # OUCH for development
  if (isTestModeQ || manualTraceModeQ()) {
    if (isTRUE(getOption("shiny.testmode"))) {
      cat(file = stderr(), "in Server: Shiny test mode\n")
    } else {
      if (manualStaticData()) {
        cat(file = stderr(), "in Server: manual static data mode\n")
      } else {
        cat(file = stderr(), "in Server: not test mode\n")
      }
    }
  }
  if (manualTraceModeQ()) {
    cat(file = stderr(), "in Server: manual TRACE mode is on\n")
  }

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
        # Set countyChoices blank
        updateSelectInput(session, 'countyChoices',
                          choices = character(0),
                          selected = NULL)
        removeUI(selector = "#CountyXX")
      }
    } else {
      # Set countyChoices blank
      updateSelectInput(session, 'countyChoices',
                        choices = character(0),
                        selected = NULL)
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
  output$vaccDataHTML <- renderUI({vaccDataHTML(input$movingAvg,
                                                input$Vaccination)})

  output$vaccRBox <- renderPlot({plotVaccBoxplots(input$movingAvg,
                                                  input$Vaccination,
                                                  input$stateChoices,
                                                  input$timeWindow)})
  output$vaccRTrend <- renderPlot({plotVaccTrend(input$movingAvg,
                                                 input$Vaccination,
                                                 input$stateChoices,
                                                 input$timeWindow)})
  output$vaccGtData <- render_gt(presentVaccData(input$movingAvg,
                                                 input$Vaccination,
                                                 input$stateChoices,
                                                 input$timeWindow),
                                 width = px(1000))

  # "New Cases" Tab
  output$newCaseHeaderHTML <- renderUI({newCaseHeaderHTML(input$movingAvg)})
  output$newCaseBox <- renderPlot({plotNewCaseBoxplots(input$chooseCounty,
                                                       input$movingAvg,
                                                       input$countyChoices,
                                                       input$stateChoices,
                                                       input$timeWindow)})
  output$newCaseTrend <- renderPlot({plotNewCaseTrend(input$chooseCounty,
                                                      input$movingAvg,
                                                      input$countyChoices,
                                                      input$stateChoices,
                                                      input$timeWindow)})
  output$newCaseBoxplotHeaderHTML <- renderUI({boxplotHeaderHTML(input$countyChoices,
                                                                 input$stateChoices)})
  output$newCaseGtData <- render_gt(presentNewCaseData(input$movingAvg,
                                                       input$countyChoices,
                                                       input$stateChoices,
                                                       input$timeWindow),
                                    width = px(1000))
  
  # "Total Cases" Tab
  output$totalCaseHeaderHTML <- renderUI({totalCaseHeaderHTML(input$movingAvg)})
  output$totalCaseBox <- renderPlot({plotTotalCaseBoxplots(input$chooseCounty,
                                                           input$movingAvg,
                                                           input$countyChoices,
                                                           input$stateChoices,
                                                           input$timeWindow)})
  output$totalCaseTrend <- renderPlot({plotTotalCaseTrend(input$chooseCounty,
                                                          input$movingAvg,
                                                          input$countyChoices,
                                                          input$stateChoices,
                                                          input$timeWindow)})
  output$totalCaseBoxplotHeaderHTML <- renderUI({boxplotHeaderHTML(input$countyChoices,
                                                                   input$stateChoices)})
  output$totalCaseGtData <- render_gt(presentTotalCaseData(input$movingAvg,
                                                           input$countyChoices,
                                                           input$stateChoices,
                                                           input$timeWindow),
                                      width = px(1000))
  
  # "New Deaths" tab    
  output$newDeathsHeaderHTML <- renderUI({newDeathsHeaderHTML(input$movingAvg,
                                                              input$countyChoices,
                                                              input$stateChoices)})
  output$newDeathsBox <- renderPlot({plotNewDeathsBoxplots(input$chooseCounty,
                                                           input$movingAvg,
                                                           input$countyChoices,
                                                           input$stateChoices,
                                                           input$timeWindow)})
  output$newDeathsTrend <- renderPlot({plotNewDeathsTrend(input$chooseCounty,
                                                          input$movingAvg,
                                                          input$countyChoices,
                                                          input$stateChoices,
                                                          input$timeWindow)})
  output$newDeathsBoxplotHeaderHTML <- renderUI({boxplotHeaderHTML(input$countyChoices,
                                                                   input$stateChoices)})
  output$newDeathsGtData <- render_gt(presentNewDeathsData(input$movingAvg,
                                                           input$countyChoices,
                                                           input$stateChoices,
                                                           input$timeWindow),
                                 width = px(1000))
  
  # "Total Deaths" tab    
  output$totalDeathsHeaderHTML <- renderUI({totalDeathsHeaderHTML(input$movingAvg,
                                                                  input$countyChoices,
                                                                  input$stateChoices)})
  output$totalDeathsBox <- renderPlot({plotTotalDeathsBoxplots(input$chooseCounty,
                                                               input$movingAvg,
                                                               input$countyChoices,
                                                               input$stateChoices,
                                                               input$timeWindow)})
  output$totalDeathsTrend <- renderPlot({plotTotalDeathsTrend(input$chooseCounty,
                                                              input$movingAvg,
                                                              input$countyChoices,
                                                              input$stateChoices,
                                                              input$timeWindow)})
  output$totalDeathsBoxplotHeaderHTML <- renderUI({boxplotHeaderHTML(input$countyChoices,
                                                                     input$stateChoices)})
  output$totalDeathsGtData <- render_gt(presentTotalDeathsData(input$movingAvg,
                                                               input$countyChoices,
                                                               input$stateChoices,
                                                               input$timeWindow),
                                 width = px(1000))
  
  # "Test Growth" tab
  output$testGrowthHeaderHTML <- renderUI({testGrowthHeaderHTML(input$chooseCounty,
                                                                input$countyChoices,
                                                                input$stateChoices)})
  output$testGBoxplotHeaderHTML <- renderUI({boxplotHeaderHTML(input$countyChoices,
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
  output$testGrowthGtData <- render_gt(presentTestGrowthData(input$movingAvg,
                                                             input$countyChoices,
                                                             input$stateChoices,
                                                             input$timeWindow),
                                 width = px(1000))
  
  # "Test Results" tab
  output$testResultsHeaderHTML <- renderUI({testResultsHeaderHTML(input$chooseCounty,
                                                                  input$countyChoices,
                                                                  input$stateChoices)})
  output$testRBoxplotHeaderHTML <- renderUI({boxplotHeaderHTML(input$countyChoices,
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
  output$testResultsGtData <- render_gt(presentTestResultsData(input$movingAvg,
                                                               input$countyChoices,
                                                               input$stateChoices,
                                                               input$timeWindow),
                                        width = px(1000))
  
  # "Summary" tab
  output$summaryHTML <- renderUI({
    summaryHTMLAny(input$chooseCounty,
                   input$countyChoices,
                   input$stateChoices)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

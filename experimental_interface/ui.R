# Author: Ryan Gifford & Dane Morey
# Organization: Cognitive Systems Engineering Lab,
#               The Ohio State University
# Year: 2022

### UI ###
#
# This file determines the user interface elements allowing the user
#   to input choices and have interactive elements from the Server file.
#

library(shiny)
library(shinyjs)

# Define UI for application that draws the visualization plots
shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),

    # Application title
    titlePanel(title = textOutput("currentCase"), windowTitle = "ANF AU22 Experiment"),

    ####################################################
    # Displays the vital sign strips visualizations
    mainPanel(
      width = 12,
      splitLayout(
        cellWidths = c('15%', '65%', '20%'),
        div(style="overflow-y: auto; position: relative", 
            plotOutput('viz_concern', width = '100%', height = '600px')),
        div(style="overflow-x: hidden", 
            verticalLayout(
              div(style="overflow-y: auto; position: relative", 
                  plotOutput("viz", width = '100%', height = '300px', click = "main_click", hover = hoverOpts("hover_ox", delay = 150, delayType = "debounce",clip=TRUE))),
              div(style="overflow-y: auto; position: relative", 
                  plotOutput("viz_hr", width = "100%", height = '300px', click = "second_click", hover = hoverOpts("hover_hr", delay = 150, delayType = "debounce",clip = TRUE))))),
        div(style="overflow-x: hidden", 
            verticalLayout(
              plotOutput("viz_demo", width = '100%', height = '50px', click = "demo_plot"),
              plotOutput("viz_labs", width = '100%', height = '88px', click = "labs_plot"),
              plotOutput("viz_meds", width = '100%', height = '88px', click = "meds_plot"),
              plotOutput("viz_ox", width = '100%', height = '88px', click = "ox_plot"),
              plotOutput("viz_bp", width = '100%', height = '88px', click = "bp_plot"),
              plotOutput("viz_resp", width = '100%', height = '88px', click = "resp_plot"),
              plotOutput("viz_hrbadge", width = '100%', height = '110px', click = "hr_plot")))
        )
      ),

    ####################################################
    # Contains the navigations options for the user
    fluidRow(
      column(2, align = "center",
             # Back button
             actionButton("backButton", label = "Back", style='padding:12px; font-size:140%')
             ),
      column(8, align = "center",
             # Slider input for the plot time frame
             sliderInput("timeframe",
                         label = h5(""),
                         width = "100%",
                         min = -24,
                         max = 0,
                         value = c(-4,0)),
             h5("Time Window (Hours)")
             ),
      column(2, align = "center",
             # Forward button
             actionButton("nextButton", label = "Next", align="center", icon=NULL, style='padding:12px; font-size:140%')
             )
      ),

    # Section divider
    hr(),
    fluidRow(
      column(2, align = "right", tags$h4("Participant ID:")),
      column(2, align = "left",
             textInput("participantID", label = NULL, value = "", placeholder = "Enter your Participant ID ...")),
      column(1, align = "right", tags$h4("Case ID:")),
      column(2, align = "left",
                          textInput("experimentID", label = NULL, value = "", placeholder = "Enter the Case ID ...")),
      column(2, align = "left", actionButton("submitButton", label = "Submit", style = 'padding:6px; font-size:100%'))
      )
    )
  )

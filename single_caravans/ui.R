library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tabsetPanel(id='tabs',
              tabPanel(h3('First results ... '), value='tab2',
                       #titlePanel("My first results ..."),
                       sidebarLayout(position='left',
                                     sidebarPanel(radioButtons("dataset", label = "Data source",
                                                               choices = list("Input map" = 1, "Softmax (single)" = 2, "Sigmoid (combine)" =3),
                                                               selected = 2, inline=TRUE),
                                       numericInput("tab2_picID", label = "Picture id (1-61):", 
                                                               value = 1, min = 1, max = 61),
                                                  textOutput("tab2_sampleAbout2"),
                                                  br(),                        
                                                  sliderInput("tab2_threshold", label = "Classification threshold",
                                                              min = 0.5, max = 1, value = .9),
                                                  radioButtons("tab2_focus", label = "Threshold focus",
                                                               choices = list("Only center" = 1, "Neighbourhood average" = 2),
                                                               selected = 2, inline=TRUE),
                                                  tags$b(textOutput('tab2_tableTitle')),
                                                  tableOutput("tab2_coordList"),
                                                  textOutput("tab2_sampleAbout1")                                                  
                                     ),
                                     mainPanel(width=5,
                                               plotOutput('tab2_samplePlot', width = 600, height = 600))
                       )),
              tabPanel(title=h3('HELP WANTED! Locate single caravans ... ') ,value='tab1',
                       #titlePanel("HELP WANTED! Locate single caravans..."),
                       sidebarLayout(position='left',
                                       sidebarPanel(numericInput("tab1_picID", label = "Picture id (1-1195):", 
                                                                 value = 1, min = 1, max = 1195),
                                                    textOutput("tab1_sampleAbout1"),
                                                    textOutput("tab1_sampleAbout2"),
                                                    br(),
                                                    tags$b(textOutput('tab1_tableTitle')),
                                                    tableOutput("tab1_coordList"),
                                                    downloadButton('tab1_downloadData', 'Save&clear')
                                       ),
                                       mainPanel(width=5,
                                                 plotOutput('tab1_samplePlot', width = 600, height = 600, click='image_click'))
                       ))
  ))) # end of fluidrow, fluidpage, shinyui
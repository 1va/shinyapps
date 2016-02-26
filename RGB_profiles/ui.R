library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
     titlePanel("RGB profiles analysis of satellite images (for the caravan parks recognition project)"),
     tabsetPanel(id='tabs',
                tabPanel('Sample RGB profiles', 
                          br(),
                          sidebarLayout(position='right',
                                       sidebarPanel(width=3, numericInput("picID", label = "Picture id (1-1195):", 
                                                                 value = 1, min = 1, max = 1195),
                                                    textOutput("sampleAbout1"),
                                                    textOutput("sampleAbout2"),
                                                    uiOutput('sampleImage')), 
                                       mainPanel(width=7, plotOutput('samplePlot')))
                          ),
                tabPanel('Classification of RGB profiles on 300x300', 
                         br(),
                         sidebarLayout(position='right', 
                                       sidebarPanel(width=5, textOutput('classAbout'),
                                                    verbatimTextOutput('summary')),
                                       mainPanel(width=7, plotOutput('clusterPlot'),
                                                 plotOutput('mdsPlot')))
                         ),
                tabPanel('Classification of RGB profiles on 24x24', 
                                  br(),
                                  sidebarLayout(position='right',
                                                sidebarPanel(width=5, textOutput('classAbout24'),
                                                             verbatimTextOutput('summary24')),
                                                mainPanel(width=7, plotOutput('clusterPlot24')))
                #         ),
                #tabPanel('2D embedding (tSNE)', 
                #         br(),
                #         sidebarLayout(position='right',
                #                       sidebarPanel(textOutput('mdsAbout')),
                #                       mainPanel(plotOutput('mdsPlot2')))
                #         ),
                #tabPanel('Temp', tableOutput('temp')
                         )
))) # end of fluidrow, fluidpage, shinyui
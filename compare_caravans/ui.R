library(shiny)
library(leaflet)
#library(RColorBrewer)

# Define UI for application that draws a histogram
shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                style="padding: .8px; background: rgba(255,255,255,0.4);",
                radioButtons("maplayer", label = "Select map layer", inline=T, 
                             choices = list("OSM" = 1, "Satellite (Esri)" = 2, "Positron" = 3), 
                             selected = 1),
                radioButtons("focus", label = "Switch focus", 
                                   choices = list("Global" = 'global', "Specific site" = 'local'),
                                   selected = 'global', inline = T),
                conditionalPanel(condition = "input.focus == 'local'",
                      uiOutput("clust_control"),
                      checkboxGroupInput("visible", label = "Visible datapoints source", 
                                   choices = list("Address Register" = 'olga', "Nnet suspect" = 'nnet'),
                                   selected = c('olga', 'nnet'),inline = T)
  ))
))




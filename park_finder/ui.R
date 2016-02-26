library(shiny)
library(leaflet)
library(RColorBrewer)

# Define UI for application that draws a histogram
shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                style="padding: .8px; background: rgba(255,255,255,0.4);",
                radioButtons("maplayer", label = "Select map layer", inline=T, 
                            choices = list("OSM" = 1, "Satellite (Esri)" = 2, "Positron" = 3), 
                            selected = 1),
                radioButtons("viewMode", label = "View mode", inline = T,
                                   choices = list("Datasource" = 1, "Naive matching" = 2),
                                   selected = 1),
                conditionalPanel( condition = "input.viewMode == 1",
                     checkboxGroupInput("visible", label = "Visible datapoints source", 
                                   choices = list("OSM " = 'osm', "Garmin " = 'garmin', 
                                                  "Karen " = 'karen',
                                                  "Neal "='neal', "77m "='m77'),
                                   selected = c('osm', 'garmin'),inline = T)),
                conditionalPanel( condition = "input.viewMode == 2",
                    checkboxGroupInput("matched", label = "Matching result (OSM-Garmin-77m)",
                                   choices = list("All 3" = 'all', "2w/o OSM" = 'osm', 
                                                  "2w/o Garmin" = 'garmin',
                                                  "2w/o 77m" = 'm77'),
                                   selected = c('all','osm', 'garmin','m77'),
                                   inline = T))
                )
))





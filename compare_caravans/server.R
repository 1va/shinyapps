library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)

load('susp_coord.RData')
coord <- susp_coord
centers <- susp_coord %>% group_by(clust) %>% summarise(lat=mean(lat), long = mean(long)) %>% arrange(clust)
max_val <- dim(centers)[1] 
pal <- rev(brewer.pal(4, "Set2"))
factpal <- colorFactor(pal, coord$sources)

shinyServer(function(input, output, session) {
  output$clust_control <- renderUI({
     sel_val <- 1
     numericInput("clust_id", label = "Site (cluster) id: ", 
                       value = 1, min = 1, max = max_val)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% 
      setView(lng = -1.5, lat = 51.5, zoom = 8) %>%
      addTiles()
  })
  
  filteredData <- reactive({
    coord %>% subset(clust==as.character(input$clust_id)) %>% subset(sources %in% input$visible) 
  })
  
  observe({
    if (input$focus=='global')
      leafletProxy("map") %>% clearMarkers() %>% clearGroup('markers') %>%
      addCircleMarkers( data = centers, lat = ~ lat, lng = ~ long, fillOpacity = 0.6, 
                        clusterOptions = markerClusterOptions(), group='markers', 
                        popup = ~ clust)
    else if (length(input$visible)==0)
      leafletProxy("map") %>% clearGroup('markers') %>% clearMarkers()
    else if (length(input$visible)==1)
      leafletProxy("map") %>% clearMarkers() %>% clearGroup('markers') %>%
      addCircleMarkers( data = filteredData(), lat = ~ lat, lng = ~ long,
                        color = ~factpal(sources), fillOpacity = 0.6, 
                        clusterOptions = markerClusterOptions(), group='markers')
    else 
      leafletProxy("map") %>% clearMarkers() %>% clearGroup('markers') %>%
      addCircleMarkers( data = filteredData(), lat = ~ lat, lng = ~ long,
                        color = ~factpal(sources), fillOpacity = 0.6, 
                        group='markers')
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    mapstr <- c('', "Esri.WorldImagery",'CartoDB.Positron')[as.numeric(input$maplayer)]
    if (mapstr=='')
      leafletProxy("map") %>%  clearTiles() %>%
      addTiles()
    else 
      leafletProxy("map") %>%  clearTiles() %>%
      addProviderTiles(mapstr)
  })
  
  observe({
    if (input$focus == 'global') 
      leafletProxy("map") %>%  setView(lng = -1.5, lat = 51.5, zoom = 8) 
    if (input$focus == 'local')  {
      lng <- centers$long[input$clust_id] 
      lat <- centers$lat[input$clust_id]
      leafletProxy("map") %>%  setView(lng = lng, lat = lat, zoom = 17) 
    }
  })
  
  
})
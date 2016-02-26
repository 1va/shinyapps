library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)

load('coord_3sources_cl.RData')
load('coord_lists_cl.RData')
sources <- factor(c(names(coord_lists_cl),'all'), levels=c(names(coord_lists_cl),'all'))
for (s in head(sources,-1)){
  coord_lists_cl[[s]][,'sources'] <- s
}
coord_list_cl <- bind_rows(coord_lists_cl)

pal <- brewer.pal(length(sources), "Set2")
factpal <- colorFactor(pal, sources)

shinyServer(function(input, output, session) {
    
    # Reactive expression for the data subsetted to what the user selected
    filterInd <- reactive({
      if (input$viewMode==1) input$visible else input$matched 
    }) 
  
    filteredData <- reactive({
      df <- if (input$viewMode==1) coord_list_cl else coord_3sources_cl
      df %>%  subset(sources %in% filterInd())  
    })
    
    output$map <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet() %>% 
        setView(lng = -1.5, lat = 51.5, zoom = 8)
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
      if (length(filterInd())==0)
        leafletProxy("map") %>% clearGroup('markers')
      else if (length(filterInd())==1)
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
    
    # Use a separate observer to recreate the legend as needed.
    observe({
      mapstr <- c('', "Esri.WorldImagery",'CartoDB.Positron')[as.numeric(input$maplayer)]
      if (mapstr=='')
        leafletProxy("map") %>%  clearTiles() %>%
          addTiles()
      else 
        leafletProxy("map") %>%  clearTiles() %>%
          addProviderTiles(mapstr)
    })
  })
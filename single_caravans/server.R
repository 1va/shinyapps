library('shiny')
library('ggplot2')
library('dplyr')
#library('tidyr')
#library('mclust')
#library('threejs')
#source('StatEllipse.R') 
library(png)
library(grid)
library(RCurl)

RGB_coords <- read.csv('RGB_coords.csv', sep=',', header=F)
assemble_ids <- read.csv('assemble_ids.csv', sep=',', header=F)
assemble <- as.matrix(read.csv('assemble_y.csv', sep=',', header=F))
assemble1 <- as.matrix(read.csv('assemble1.csv', sep=',', header=F))
assemble4 <- as.matrix(read.csv('assemble4.csv', sep=',', header=F))
NUM_OBS <- dim(assemble)[1]
#tempIMG <- tempfile(fileext = ".png")

shinyServer(function(input, output, session) {
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~Shared variables and reactive enviroments  
  values <- reactiveValues(click_list = matrix(nrow=0,ncol=2, dimnames=list(NULL,c('x','y'))) )  
  picID <- reactive({ 
    #returns the active picID - making sure its valid number
    values$click_list <- matrix(nrow=0,ncol=2, dimnames=list(NULL,c('x','y'))) 
    if (input$tabs=='tab2') num <- min(input$tab2_picID, NUM_OBS) else num <- input$tab1_picID
    num <- if (is.numeric(num)) num else 1
    return(round(max(1,num)))
  })
  
  coords <- reactive({
   if (input$tabs=='tab1')   paste0(RGB_coords[picID(),3],",",RGB_coords[picID(),2])
   else paste0(assemble_ids[picID(),3], ',', assemble_ids[picID(),2])
   })
  
  
  sampleURL <- reactive({
    #url for the google static map at given GPS coordinates
    beginning = "https://maps.googleapis.com/maps/api/staticmap?"
    api_key = 'key=AIzaSyADEXfeHpcdEXfsEWF_F7iX5bKzVSnlkk8'
    center = paste0("center=",coords())
    zoom = "zoom=18" 
    img_format = "format=png"
    map_type = "maptype=satellite"
    imag_size = 'size=600x600'
    txt <- paste(beginning, center, zoom, img_format, map_type, imag_size, api_key, sep='&')
    return(txt)
  })
    
  newimage <- reactive({
    #download.file(sampleURL(),tempIMG,mode="wb")
    #img <- readPNG(tempIMG)
    values$click_list <- matrix(nrow=0,ncol=2, dimnames=list(NULL,c('x','y'))) 
    img <-  readPNG(getURLContent(sampleURL()))
    return(rasterGrob(img))
  })
  
  square_list <- reactive({
    if (dim(values$click_list)[1]==0)
      return(data.frame(x=-10,y=-10, group=0))
    else {
      s<-10
      res <- matrix(nrow=0,ncol=3, dimnames=list(NULL,c('x','y', 'group')))
      for (i in 1:dim(isolate(values$click_list))[1]) {
        x<-isolate(values$click_list)[i,1]
        y<-isolate(values$click_list)[i,2]
        res <- rbind(res, cbind(x=c(x-s, x-s, x+s, x+s, x-s), y=c(y-s, y+s, y+s, y-s, y-s),group=i))
      }  
      #print(res)
      return(as.data.frame(res)) 
    }
  })

  keep_sites <- reactive({
    #estimate number of sites
    #count local maxima above some threshold
    n<-70
    if (input$dataset==1)  prob <- assemble[picID(),]
    else if (input$dataset==2) prob <- assemble1[picID(),]
    else prob <- assemble4[picID(),]
    x <- matrix(prob, n,n)
    z <- (x[3:n, ] + x[2:(n-1), ] + x[1:(n-2), ])/3
    z <- (z[ , 3:n] + z[ , 2:(n-1)] + z[ , 1:(n-2)])/3
    x[2:(n-1),2:(n-1)] <- x[2:(n-1),2:(n-1)] + matrix(rnorm(length(z), 0, z/1000000),n-2)               # to resolve ties
    y <- pmax(x[3:n, ], x[2:(n-1), ], x[1:(n-2), ])
    y <- pmax(y[ , 3:n], y[ , 2:(n-1)], y[ , 1:(n-2)])
    keep <- matrix(0,n,n)
    if (input$tab2_focus==1) z <- y 
    keep[2:(n-1),2:(n-1)] <- y==x[2:(n-1),2:(n-1)] & z>input$tab2_threshold
    return(keep==1)
  })

  predictions <- reactive({
    n <- 70
    if (input$dataset==1)  prob <- assemble[picID(),]
    else if (input$dataset==2) prob <- assemble1[picID(),]
    else prob <- assemble4[picID(),]
    x <- 9+4*rep(1:n,n)
    y <- rep(9+4*(n:1),each=n)
    return(data.frame(x,y,prob, keep = c(keep_sites())))
  })

#~~~~~~~~~~~~~~~~~~~~~~~~~tab1 outputs HELP WANTED: 
  output$tab1_sampleAbout1 <- renderText({
    paste("This is ", if (RGB_coords[picID(),1]==0) 'not ' else '', 'a caravan park.')
  })
  output$tab1_sampleAbout2 <- renderText({
    paste('Coordinates: ', coords())
  })
      
  output$tab1_downloadData <- downloadHandler(
    filename = function() {
      sprintf('click/%4d_hitlist.csv', isolate(picID()))
    },
    content = function(con) {
      write.table(isolate(values$click_list), con, append = FALSE, quote = FALSE, sep = ",",
                row.names = FALSE, col.names = TRUE)
    })
  
  output$tab1_tableTitle <- renderText({'Found caravans at pixel coordinates:'})

  output$tab1_coordList <- renderTable({
    #click_list() <- reactive({
    values$click_list <- rbind(isolate(values$click_list), 
                               c(as.integer(input$image_click$x), as.integer(input$image_click$y)))
    return(values$click_list)
  })
  
  output$tab1_samplePlot <- renderPlot({
      ggplot(data=square_list(),aes(x=x,y=y, group=group)) +
        xlim(0, 300) + ylim(0,300) +
        theme(aspect.ratio = 1) +
        annotation_custom(newimage(), xmin=0, xmax=300, ymin=0, ymax=300) +
        geom_path(colour='red') 
    },width = 800, height = 800)
  
  
#~~~~~~~~~~~~~~~~~~~~~~~tab2 outputs First results:   
  output$tab2_sampleAbout2 <- renderText({
    paste('Image ', assemble_ids[picID(),1], ' with coordinates: ', coords())
  })

  output$tab2_samplePlot <- renderPlot({
    ggplot(data=predictions(),aes(x=x,y=y, z=prob)) +
      xlim(0, 300) + ylim(0,300) +
      theme(aspect.ratio = 1) +
      annotation_custom(newimage(), xmin=0, xmax=300, ymin=0, ymax=300) +
      geom_contour(aes(colour= ..level..), breaks = (1:10)/10) + # 1-((0:5)/5)^5) +
      geom_point(aes(alpha=keep), colour='blue') +
      scale_colour_gradient(low = "green", high = "red")   
  },width = 800, height = 800)
  
  output$tab2_tableTitle <- renderText({
    paste("This is ", if (sum(keep_sites()) == 0) 'not ' else '', 'classified as a caravan park.')
    })

  output$tab2_sampleAbout1 <- renderText({
    paste('Max score = ', max(predictions()$prob), 
          ' Median score = ', median(predictions()$prob),
          ' Num of blue sites = ', sum(predictions()$keep))
    }) 
})

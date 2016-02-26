library('shiny')
library('ggplot2')
library('dplyr')
library('tidyr')
#library('mclust')
#library('threejs')
#source('StatEllipse.R') 

RGB_profiles <- read.csv('RGB_profiles.csv', sep=',', header=F)
RGB_coords <- read.csv('RGB_coords.csv', sep=',', header=F)
tsne_coord <- read.csv('tsne_8mean.csv', sep=',', header=F)

NUM_OBS <- dim(RGB_profiles)[1]
NUM_CHANNELS <- 3
PIXEL_DEPTH <- dim(RGB_profiles)[2]/NUM_CHANNELS
VAR_TYPES <- factor(c('red', 'green','blue'), levels=c('red', 'green','blue'))

tmp <- t(read.csv('RGB_means.csv', sep=',', header=F))
RGB_means <- data.frame(tmp[2:769,])
colnames(RGB_means) <- tmp[1,]

#Prepare variables before reshaping dataframes
Density <- rep(0:(PIXEL_DEPTH-1),each = NUM_CHANNELS)
Color <- rep(VAR_TYPES, PIXEL_DEPTH)
Caravan <- factor(RGB_coords[,1], levels=c(1, 0), labels = c('Caravans: True', 'Caravans: False'))
RGB_cumsum <- t(apply(RGB_profiles, 1, function(x) t(simplify2array(tapply(x , Color, cumsum)))))
RGB_8mean <- t(apply(RGB_profiles, 1, tapply, rep(1:(32*3), each=8), mean))
#RGB_quantile <- t(apply(RGB_profiles[,-1],1,function(x) 
#    t(simplify2array(tapply(x , Color, function(y) 
#    quantile(rep(1:PIXEL_DEPTH ,  y),(1:256)/257))))))

#hcEUCLID <- hclust(dist(x), method='ward.D')
#mdsEUCLID <- cmdscale(dist(RGB_profiles[,-1])) 

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  picID <- reactive({ 
    #returns the active picID - making sure its valid number
    num <- if (is.numeric(input$picID)) input$picID else 1
    return(round(min(max(1,num),NUM_OBS)))
  })
  
  sampleURL <- reactive({
    #url for the google static map at given GPS coordinates
    beginning = "https://maps.googleapis.com/maps/api/staticmap?"
    api_key = 'key=AIzaSyADEXfeHpcdEXfsEWF_F7iX5bKzVSnlkk8'
    center = paste0("center=",RGB_coords[picID(),3],",",RGB_coords[picID(),2])
    zoom = "zoom=17" 
    img_format = "format=png"
    map_type = "maptype=satellite"
    imag_size = 'size=300x300'
    txt <- paste(beginning, center, zoom, img_format, map_type, imag_size, api_key, sep='&')
    return(txt)
  })
    
  output$sampleAbout1 <- renderText({
    paste("This is ", if (RGB_coords[picID(),1]==0) 'not ' else '', 'a caravan park.')
  })
  output$sampleAbout2 <- renderText({
    paste('Coordinates: ', RGB_coords[picID(),3], ', ', RGB_coords[picID(),2])
  })
  
  output$sampleImage <-renderUI({
    # embed the picture grom google map
    tags$img(src= sampleURL())
  })

  sampleData <- reactive({
    #create a dataframe of RGB values for a specific picture
    numPixels <- c(t(RGB_profiles[picID(),]), RGB_cumsum[picID(),])   #, RGB_quantile[picID,])
    Type <- factor(rep(c('Pixel counts', 'Cummulative pixel counts'#,'Quantiles'
    ), each=NUM_CHANNELS*PIXEL_DEPTH), levels=c('Pixel counts', 'Cummulative pixel counts'))
    df <- data.frame(numPixels, Density, Color, Type)
    return(df)
  })
  
  output$samplePlot <- renderPlot({
    ggplot(sampleData(), aes(x = Density)) +
      #geom_point(size = 4, shape = 20) +
      geom_bar(aes(weight = numPixels/6,  fill=Color), binwidth = 6, alpha = .3) +
      stat_smooth(aes(y = numPixels, color= Color), method = 'loess', span = .3, se = F) +
      facet_grid(Type ~ Color, scales = 'free') +
      guides(fill = FALSE, color = FALSE) 
  })  
    
  clusterData <- reactive({
    #prepare dataframe with RGB data averaged over clusters
    numPixels <- c(t(as.matrix(RGB_profiles)))
    df <- data.frame(numPixels, Color, Density, 
                     Caravan = rep(Caravan, each = length(Density))) 
    df_mean <- aggregate(numPixels ~., data=df, mean)
    return(df_mean)  
  })
  
  output$clusterPlot <- renderPlot({
    ggplot(clusterData(), aes(x=Density)) + #, color=Color)) +
    geom_bar(aes(weight = numPixels/6,  fill=Color), binwidth = 6, alpha = .3) +
    #stat_summary(fun.y='median', geom="line") +
    stat_smooth(aes(y=numPixels, color = Color), method ='loess', span = .3, se = F) +   
    facet_grid(Caravan ~ Color)   +
    guides(fill = FALSE, color = FALSE) +
    ggtitle("Mean RGB profiles by classification")
  })
  
  output$mdsPlot <- renderPlot({
    tsne_df <- data.frame(tsne_coord, Caravan = factor(Caravan,labels=c('True','False')))
    #print(str(tsne_df))
    ggplot(tsne_df, aes(x=V1, y=V2, color=Caravan)) +
      geom_point() +
      scale_color_brewer(palette="Set1") +
      #scale_color_manual(values=c("red", "#3366CC")) +
      guides(color=guide_legend("Caravans")) +
      ggtitle("2D embedding of data points (using tSNE)")
  })
  
  meanData <- reactive({
    numPixels <- c(RGB_means[,1], RGB_means[,2])
    tmp <- factor(rep(c(0,1), each=length(Color)), levels=c(1, 0), labels = c('Caravans: True', 'Caravans: False'))
    df <- data.frame(numPixels, Density, Color, Caravan=tmp)
    return(df)
    })
  
  output$clusterPlot24 <- renderPlot({
    ggplot(meanData(), aes(x=Density)) + #, color=Color)) +
      geom_bar(aes(weight = numPixels/6,  fill=Color), binwidth = 6, alpha = .3) +
      #stat_summary(fun.y='median', geom="line") +
      stat_smooth(aes(y=numPixels, color = Color), method ='loess', span = .3, se = F) +   
      facet_grid(Caravan ~ Color)   +
      guides(fill = FALSE, color = FALSE) +
      ggtitle("Mean RGB profiles by classification")
  })
  
  
  output$classAbout <- renderText({
    paste('Summary of supervised classification methods:')
  })
  output$summary <- renderText({
    paste(
      '*** Smooth RGB profiles using linear kernel of bandwidth 8.',
      'PCA - number of components needed to explain 95% variability: 43.','',
      '*** Logistic Regression (l2 penalty):',
      'full: Accuracy: 72.6%, F1-score: 71.6%','      Contingency table: (TP = 422, FP = 153, FN = 174, TN = 446)',
      'pca:  Accuracy: 65.8%, F1-score: 64.1%','      Contingency table: (TP = 373, FP = 186, FN = 223, TN = 413)','',
      '*** Random Forest (entropy criterion):',
      'full: Accuracy: 71.5%, F1-score: 70.3%','      Contingency table: (TP = 413, FP = 157, FN = 183, TN = 442)',
      'pca:  Accuracy: 69.6%, F1-score: 68.1%','      Contingency table: (TP = 397, FP = 164, FN = 199, TN = 435)','',
      '*** Quadratic Discriminant Analysis (collinearity warning):',
      'full: Accuracy: 51.3%, F1-score: 15.9%','      Contingency table: (TP = 56, FP = 42, FN = 540, TN = 557)',
      'pca:  Accuracy: 68.8%, F1-score: 64.6%','      Contingency table: (TP = 347, FP = 124, FN = 249, TN = 475)','',
      '*** Gaussian Naive Bayes:',
      'full: Accuracy: 63.3%, F1-score: 56.6%','      Contingency table: (TP = 292, FP = 135, FN = 304, TN = 464)',
      'pca:  Accuracy: 64.5%, F1-score: 57.2%','      Contingency table: (TP = 289, FP = 117, FN = 307, TN = 482)','',
      '*** Linear Support Vector Machine (l1 penalty):',
      'full: Accuracy: 72.1%, F1-score: 71.4%','      Contingency table: (TP = 427, FP = 164, FN = 169, TN = 435)',
      'pca:  Accuracy: 65.7%, F1-score: 64.2%','      Contingency table: (TP = 375, FP = 189, FN = 221, TN = 410)',
      sep =  "\n")
  })
  
  
  output$classAbout24 <- renderText({
    paste('Summary of supervised classification methods:')
  })
  output$summary24 <- renderText({
    paste(
      '*** Smooth RGB profiles using linear kernel of bandwidth 8.',
      'PCA - number of components needed to explain 95% variability: 52.','',
      '*** Logistic Regression (l2 penalty):',
      'full: Accuracy: 96.6%, F1-score: 95.8%','      Contingency table: (TP = 1920, FP = 94, FN = 55, TN = 2306).',
      'pca:  Accuracy: 87.6%, F1-score: 85.2%','      Contingency table: (TP = 1622, FP = 189, FN = 353, TN = 2211)','',
      '*** Random Forest (entropy criterion):',
      'full: Accuracy: 95.7%, F1-score: 94.7%','      Ccontingency table: (TP = 1864, FP = 77, FN = 111, TN = 2323)',
      'pca:  Accuracy: 89.6%, F1-score: 87.3%','      Contingency table: (TP = 1648, FP = 130, FN = 327, TN = 2270)','',
      '*** Quadratic Discriminant Analysis (collinearity warning):',
      'full: Accuracy: 96.1%, F1-score: 95.1%','      Contingency table: (TP = 1822, FP = 16, FN = 153, TN = 2384)',
      'pca:  Accuracy: 87.2%, F1-score: 86.3%','      Contingency table: (TP = 1844, FP = 431, FN = 131, TN = 1969)','',
      '*** Linear Discriminant Analysis::',
      'full: Accuracy: 96.7%, F1-score: 95.9%','      Contingency table: (TP = 1936, FP = 107, FN = 39, TN = 2293)',
      'pca:  Accuracy: 86.1%, F1-score: 83.3%','      Contingency table: (TP = 1574, FP = 208, FN = 401, TN = 2192)','',
      '*** Gaussian Naive Bayes::',
      'full: Accuracy: 85.2%, F1-score: 82.9%','      Contingency table: (TP = 1629, FP = 301, FN = 346, TN = 2099)',
      'pca:  Accuracy: 67.6%, F1-score: 70.5%','      Contingency table: (TP = 1736, FP = 1179, FN = 239, TN = 1221)','',
      '*** Linear Support Vector Machine (l1 penalty):',
      'full: Accuracy: 97.0%, F1-score: 96.2%','      Contingency table: (TP = 1922, FP = 80, FN = 53, TN = 2320)',
      'pca:  Accuracy: 87.1%, F1-score: 84.5%','      Contingency table: (TP = 1596, FP = 184, FN = 379, TN = 2216)','',
      sep =  "\n")
  })

})
    
    
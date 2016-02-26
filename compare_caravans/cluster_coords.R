library(dplyr)

#helper functions:
latlong2milesmatrix <- function(lat,long){
  as.matrix(cbind((lat-50)/.015, long/0.23))
}

#####################################################
#compare olga and nne soutwest lists

unique_ind <-  function(x, k=5){
    x <- round(x,k) %>% arrange(V1,V2)
    ind <-  x[2:dim(x)[1], ] == x[1:(dim(x)[1]-1), ]
    return( ind[,1]*ind[,2] == 0 )
    }

suspect_clust <- function(olga_file = 'Olgas_list_gps.csv', nnet_file = 'suspect_southwest-3.csv'){
  olga <- read.csv(olga_file, sep=',', header=F)
  nnet <- read.csv(nnet_file, sep=',', header=F)
  #clean map overlap for neural net output
  nnet <- nnet[unique_ind(nnet), ]
  
  coord <- rbind( nnet, olga)
  colnames(coord)<-c('long','lat')
  coord$sources <- factor(c(rep('nnet', dim(nnet)[1]), rep('olga', dim(olga)[1]) ))
  
  # there will be a need to split the data before calling 'dist'  !!!
  coord<-coord[coord$lat<51.3,]
  
  # add clustering data 
  d <- dist(latlong2milesmatrix(coord$lat, coord$long))
  cl<- hclust(d,method='single')
  coord$clust <- cutree(cl,h=.03)
  
  # keep only extremely suspicious sites
  suspect <- coord %>% group_by(clust) %>% summarise(susp = sum(sources=='nnet')-sum(sources=='olga')>10)
  k <- sum(suspect$susp)  # number of suspicious sites
  print(paste('Num of suspicious clusters: ', k))
  keep <- suspect$clust[suspect$susp]
  susp_coord <- coord[coord$clust %in% keep,]
  susp_coord$clust <- factor(susp_coord$clust, labels=1:length(unique(susp_coord$clust)))
  return(susp_coord)
  }

###################################################
#run the above defined function

setwd("~/R/shiny-apps/Olgas_list")


susp_coord <- suspect_clust()

save(susp_coord, file='susp_coord.RData')



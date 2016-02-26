library(dplyr)
setwd("~/R/shiny-apps/Temp CSV")

osm <- read.csv('GPS_osm_596caravans.csv', sep=',', header=F)
names(osm) <- c('lat','long')

garmin1 <- read.csv('garmin poi list/Camp_UK-England.csv', sep=',', header=F)
garmin2 <- read.csv('garmin poi list/Camp_UK-Scotland.csv', sep=',', header=F)
garmin3 <- read.csv('garmin poi list/Camp_UK-Wales.csv', sep=',', header=F)
garmin <- data.frame(rbind(garmin1, garmin2, garmin3))[, c(2,1,3)]
garmin[,3] <- as.character(substr(garmin[,3],1,25))
names(garmin)<-c('lat', 'long', 'desc')

karen <- read.csv('Karens_park_homes.csv', sep=',', header=T)
karen <- karen[karen[,'parkhome']==1,c(3,4,2)]
karen[,3] <- as.character(karen[,3])
names(karen) <- c('lat','long', 'desc') 

neal1 <- read.csv('Neals_List1_final.csv', sep=',', header=T)
neal2 <- read.csv('Neals_List1_final_gps.csv', sep=',', header=F)
neal<-data.frame(lat=neal2$V2, long=neal2$V1, desc=neal1$POSTCODE_LOCATOR)

m771 <- read.csv('77m_Sample_list.csv', sep=',', header=T)
m772 <- read.csv('77m_Sample_list_gps.csv', sep=',', header=F)
m77 <- data.frame(lat=m772$V2, long=m772$V1, desc=m771$postcode)
m77 <- m77[m77[,1]<Inf,]

coord_lists <- list(osm, garmin, karen, neal, m77)
names(coord_lists)<-c('osm', 'garmin', 'karen', 'neal', 'm77')
save(coord_lists, file= 'coord_lists.RData')

####################################################

load('coord_lists.RData')

cluster_sites <- function(df, tresh = .0003, method='single'){
  ndf <- df %>% group_by(lat,long) %>% summarize(count=n()) 
  d <- dist(ndf[,c('lat','long')])
  cl<- hclust(d,method)
  ndf[,'cluster'] = cutree(cl, h=tresh)
  df <- ndf %>% group_by(cluster) %>% summarize(lat=mean(lat), long=mean(long), num_sites=sum(count))
  return(df[,-1])
}


tresh <- c(0.0001, 0.0003, 0.0006, 0.001, .003, .006, .01, .015, .02)
system.time(parks_num <- cbind(sapply(coord_lists, dim)[1,],
                  sapply(tresh, function(x) 
                         sapply(lapply(coord_lists, cluster_sites, t=x), dim)[1,] )))
colnames(parks_num) <- c('total',paste('t=',tresh))
print(parks_num)

#choice of treshold: .003 (cuts significantly neals&m77 which list all caravans in opposite to just parks)
#warning: the dataset with more obs are more dense and therefore sensitive to high treshold 

coord_lists_cl <- lapply(coord_lists, cluster_sites, t=.001) 
save(coord_lists_cl, file= 'coord_lists_cl.RData')

#####################################
load('coord_lists_cl.RData')
sources <- factor(names(coord_lists_cl), levels=names(coord_lists_cl))
for (s in sources){
  coord_lists_cl[[s]][,'sources'] <- s
}
df <- bind_rows(coord_lists_cl) %>% subset(sources %in% c('osm', 'garmin', 'm77'))

match_clusters <- function(df, tresh = .003, method='single'){
  d <- dist(df[,c('lat','long')])
  cl<- hclust(d,method)
  df[,'cluster'] = cutree(cl, h=tresh)
  df <- df %>% group_by(cluster) %>% 
         summarize(lat = mean(lat), long = mean(long), num_sites = n(), 
                   osm = sum(sources=='osm')>0, 
                   garmin = sum(sources=='garmin')>0,
                   m77 = sum(sources=='m77')>0 )
  return(df[,-1])
}

dff <- match_clusters(df) %>% 
         subset(osm+garmin+m77>1) %>% 
         mutate(all = (osm+garmin+m77)<3)         
dff[dff==T] <- NA
library(tidyr)
coord_3sources_cl <- dff %>% 
      gather(key = sources, value = nic, osm, garmin, m77, all, na.rm=T) %>%
      select(-nic)

save(coord_3sources_cl, file= 'coord_3sources_cl.RData')

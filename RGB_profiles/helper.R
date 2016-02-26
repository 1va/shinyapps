setwd("~/R/shiny-apps/RGBcurves")
RGB_profiles <- read.csv('RGB_profiles.csv', sep=',', header=F)

RGB_8mean <- t(apply(RGB_profiles, 1, tapply, rep(1:(32*3), each=8), mean))

d <- dist(RGB_8mean)
library(tsne)
tsne_8mean <- tsne(d)
write.table(tsne_8mean, 'tsne_8mean.csv', sep=',', row.names=F, col.names=F)

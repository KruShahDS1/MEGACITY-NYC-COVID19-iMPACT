install.packages("sf")
library(sf)
library(ggplot2)
install.packages("tmap")
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)

# Project Comcast Complaints 
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(xtable)
library(tidyr)          # for data manipulation
library(magrittr)       # for easier syntax in one or two areas
library(gridExtra)      # for generating some comparison plots
library(ggplot2)

install.packages("ggrepel")
library(ggrepel)

options(scipen = 99)
setwd("F:/FileHistory/Datathon_Project_2020/Datasets/NewFiles_Bo/")
getwd()
mymap <- st_read("geo_export_1a561008-d833-42bd-8af0-c3e7f33bf4d2.shp", stringsAsFactors=TRUE)
mydata <- read.csv("data_gisR.csv", header=TRUE)

str(mymap)
str(mydata)
newdata <- na.omit(mydata)
map_and_data <- inner_join(mymap, newdata)

###########  Maps

borough_points<- st_centroid(mymap)
ggplot(map_and_data) +
  geom_sf(aes(fill = Asthma)) +
  scale_fill_gradient(low = "56B1F7", high="132B43")

##summary statistics table (1 - by age group and 2 - overall)
table1::table1(~Asthma + COVI19.total + Air.Quality  + Toxic.Chemicals + SNAP	+ Commute.Time + Diabetes + Elderly.Poverty.share, data = newdata) 


install.packages("spdep")
library(spdep)
summary(newdata)
cor.test(newdata$COVI19.total,newdata$Commute.Time)
cor.test(newdata$COVI19.total,newdata$Air.Quality)
cor.test(newdata$COVI19.total,newdata$SNAP)
cor.test(newdata$COVI19.total,newdata$Elderly.Poverty.share)
cor.test(newdata$COVI19.total,newdata$Asthma)

cor.test(newdata$COVI.19.cases1000,newdata$Asthma)
cor.test(newdata$COVI.19.cases1000,newdata$Commute.Time)
cor.test(newdata$COVI.19.cases1000,newdata$Elderly.Poverty.share)

moran.test(newdata$Asthma, lw)





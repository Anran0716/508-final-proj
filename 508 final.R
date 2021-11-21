library(tidyverse)
library(sf)
library(RSocrata)
library(viridis)
library(spatstat)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
# functions
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

cin_overdose<- st_read("C:/Users/zheng/Desktop/508 final/Cinci_Overdoses.geojson") %>%
  st_transform('ESRI:102258')

#build prediction dataset with test=0 (year2016)
drug16<-cin_overdose%>%
  filter(test==0)%>%
  st_transform('ESRI:102258')

#build test dataset with test=0 (year2017)
drug17<-cin_overdose%>%
  filter(test==1)%>%
  st_transform('ESRI:102258')

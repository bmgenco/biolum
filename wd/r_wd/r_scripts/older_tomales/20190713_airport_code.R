# aiport code.


rm(list=ls())
#setwd("C:/Users/brandon/Desktop/temp_tomales/working_directory")
# explore data object





setwd("Z:\\Personal\\Brandon\\working_directory")
s<-readRDS("SAM_8586_CALIBRATED.RDS")
data<-read.csv("output/SAM_8586_CALIBRATED_2019-06-16141420.csv")
night<-data[244:374,]

x=345

s[x,3][[1]]

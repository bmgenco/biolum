  
  rm(list=ls())
  
  f.ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  packages <- c("sp", "rgdal", "extrafont", "maptools", "maps", "mapproj", "rgeos", "ggplot2", "raster", "marmap", "geoR", "lattice") #, "colorspace")
  f.ipak(packages)
  rm(packages)
  
  wd<-"/home/brandon/labserv/Personal/Brandon/working_directory/temp"
  data1<-"/home/brandon/labserv/Personal/Brandon/GIS/tomales bay/noaa chart/shape_file/"
  data2<-"/home/brandon/labserv/Personal/Brandon/Dissertation/tomales/field_site_data/processed_data/June_2019/"
  setwd(wd)
  
#new:
wd<-"/home/brandon/labserv/Personal/Brandon/working_directory/wd_tomales_2019"
setwd(wd) 
  
  
  ### data load:
  stations<-readOGR((file.path(data1,"US5CA96M/")),"SBDARE-point")
  day1<-readOGR((file.path(data2)),"tomales_20190616_day")
  night<-readOGR((file.path(data2)),"tomales_20190616_night")
  day2<-readOGR((file.path(data2)),"tomales_20190617_day")
  lid<-raster("/home/brandon/labserv/Personal/Brandon/GIS/tomales bay/lidar_dem_1984.tif")
  lid<-crop(lid, extent(-122.9911,-122.89, 38.165, 38.24668 ))
  lid<-as.bathy(lid)
  noaa_bath<-raster("/home/brandon/labserv/Personal/Brandon/GIS/tomales bay/rectag_subset_bathy.tif")
  
  # fixinging data:
  
  names(day1@data)[names(day1@data) == 'Comment'] <- 'site'
  names(day2@data)[1]<-"site"
  names(night@data)[1]<-"site"
  
  day1@data$site<-as.character(day1@data$site)
  day2@data$site<-as.character(day2@data$site)
  night@data$site<-as.character(night@data$site)
  

  
### creating d atate for export
  
library(tidyverse)

mstats<-cbind(as.character(stations@data$OBJNAM), stations@coords[,2], stations@coords[,1])
#attributes(mstats)<-NULL

buoy<-day1@data[8,c(1,6,7)]
buoy$site[1]<-("science buoy")
buoy$site<-as.character(buoy$site)
#attributes(buoy)<-NULL

bo<-night@data[2,c(1,6,7)]
bo$site<-as.character(bo$site)
#attributes(bo)<-NULL
 
test<-data.frame(mstats, ncol=3)

names(mstats)<-names(buoy)
stations.all<-data.frame(rbind(as.matrix(mstats), as.matrix(buoy)))
stations.all<-data.frame(rbind(stations.all, as.matrix(bo)))
setwd("/home/brandon/Dropbox/MERCED/tomales_summer_2020/june_trip/gps")
write_csv2(stations.all, "stations.csv")


test<-rbind(mstats, buoy)


mstatscbind(mstats, buoy)

stations.all<-c(mstats, bo)

 # LLcoor<-spTransform(WGScoor,CRS("+proj=longlat"))
  
  #temp<-unclass(lid)
  
  
   ### plots:
  
  ramp<-c("#2D3184", "#254289", "#1C518F", "#115F96", "#046C9C", "#0078A2", "#0084A7",
          "#068FAB", "#189AAF", "#28A4B3", "#45B6B8", "#54BEBA", "#62C5BC", "#70CCBD", "#7DD2BF", 
          "#8BD8C0", "#97DDC1", "#A3E2C3", "#AFE5C4", "#BAE9C6", "#C4EBC8", "#CDEECA", "#D6F0CD",
          "#DEF1D0", "#E4F2D3", "#EAF2D6", "#EEF2DA", "#F2F2DE", "#F3F1E4")
  
  
#pdf("test3.pdf", res=600)
    #plot(lid, bpal=ramp,  image=T, deep=c(-40,0), shallow=c(-1,0), step=c(10,0), lwd=c(0.4,0.8), lty=c(1,1))
    #scaleBathy(lid, x="bottomleft")
    #points(night, pch=21, col="orange",bg=col2alpha("red",.4),cex=1)
  dev.off()
  
  
  #rm(ramp)
  #
  
  # 3d ->
  fig1<-wireframe(unclass(lid), shade=T, aspect=c(1/2, 0.1),
            screen = list(z = 0, x = -50),
            #par.settings = list(axis.line = list(col = "transparent")),
            #par.box = c(col = rgb(0,0,0,0.1)))
            )
 
    setwd(wd)
    png(filename = "test_2.png", res=600)
    fig1
    dev.off()
  

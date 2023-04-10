# add staion information to 
rm(list=ls())

# directories
rDir<-"Z:\\Personal\\Brandon\\Dissertation\\tomales\\field_site_data\\ramses_data_tomales_bay\\201906"
rDir<-"/home/brandon/labserv/Personal/Brandon/Dissertation/tomales/field_site_data/ramses_data_tomales_bay/201906/"
dirL <- list.files(rDir, full.names = T, pattern = ".dat$", recursive = T)
outDir<-"Z:\\Personal\\Brandon\\working_directory\\output"

#gisdata<-("Z:/Personal/Brandon/Dissertation/tomales/field_site_data/trimble_data_tomales_bay/june_2019/tom/")

require(sp)
require(rgdal)
require(rgeos)

#shp<-readOGR((file.path(gisdata, "/Point_generic")), "Point_generic")
shp<-readOGR("/home/brandon/labserv//Personal/Brandon/Dissertation/tomales/field_site_data/trimble_data_tomales_bay/june_2019/tom/Point_generic.shp")


#temp plotting
shp@data$names<-shp@data$Comment
shp@data$names<-as.character(shp@data$names)
plot(shp)
text(shp, shp@data$names, cex=.75, pos=3)


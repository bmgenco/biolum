
#### start here!!! ####

rm(list=ls())




## require packages
require(plyr)
require(tidyverse)
require(tibble)
require(lubridate)
require(stringr)
require(tools)
require(ggplot2)
require(devtools)
require(patchwork)

#outDir<-"Z:\\Personal\\Brandon\\working_directory\\output"
#setwd("Z:\\Personal\\Brandon\\working_directory")
#newoutDir<-"Z:\\Personal\\Brandon\\working_directory\\new_moon_output"



outDir<-"/home/brandon/labserv/Personal/Brandon/working_directory/wd_tomales_2019/output"
newoutDir<-"/home/brandon/labserv/Personal/Brandon/working_directory/wd_tomales_2019/new_moon_output"
wd<-"/home/brandon/labserv/Personal/Brandon/working_directory/wd_tomales_2019"
setwd(wd)

### load files for for loops ####

file.r <-list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8586_CALIBRATED.*csv$")
file.i<-list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8589_CALIBRATED.*csv$")
file.is<-list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8581_CALIBRATED.*csv$")


nm.file.r <-list.files(newoutDir, full.names = T, recursive = T, pattern = "SAM_8586_CALIBRATED.*csv$")
nm.file.i<-list.files(newoutDir, full.names = T, recursive = T, pattern = "SAM_8589_CALIBRATED.*csv$")
nm.file.is<-list.files(newoutDir, full.names = T, recursive = T, pattern = "SAM_8581_CALIBRATED.*csv$")


# fix index out side function


### change index here for approriate full moon ####
index<-read.csv("june2019_sampling_index.csv")

w<- 8 # night stations above water
w<- 9 # night stations submerged

index.x<-droplevels(index[c(1:4,7,8,10),c(1,w)]) # night stations

names(index.x)<-c("station", "index")

index.x$index<-as.character(index.x$index)
index.x$station<-as.character(index.x$station)

index<-separate(index.x, "index", paste("index", 1:2, sep=" "), sep=":", extra="drop")
index[,2]<-as.numeric(index[,2])
index[,3]<-as.numeric(index[,3])
rm(index.x)



### change index here for approriate NEW moon ####

index<-read.csv("may2019_sampling_index.csv")
w<- 4 # night stations above water
w<- 5 # night stations submerged


index.x<-droplevels(index[c(1:3),c(1,w)]) # night stations
names(index.x)<-c("station", "index")

index.x$index<-as.character(index.x$index)
index.x$station<-as.character(index.x$station)

index<-separate(index.x, "index", paste("index", 1:2, sep=" "), sep=":", extra="drop")
index[,2]<-as.numeric(index[,2])
index[,3]<-as.numeric(index[,3])
rm(index.x)


### loops 1 - full moon #### 

#full moon

fm.night.reflec<- vector("list", 7)
for (i in 1:dim(index)[1]){

  
j<-index[i,2]
l<-index[i,3]

r.f<-file.r[j:l]
i.f<-file.i[j:l]

rad<-list()
irad<-list()

for (q in 1:length(r.f)){
  rad[[q]] <- read.csv(r.f[q])}
for (k in 1:length(i.f)){
  irad[[k]] <- read.csv(i.f[k])}
f.1<-function(x){x[,2][x[,2] < 0] <- NA
return(x[28:118,1:2])}

rad.1<-lapply(rad, f.1)
irad.1<-lapply(irad, f.1)

med.rad<-apply(laply(rad.1, as.matrix), c(2,3), median)
med.irad<-apply(laply(irad.1, as.matrix), c(2,3), median)

rownames(med.rad) <- c()
rownames(med.irad) <- c()

reflec<- as.data.frame(cbind(med.rad[,1], ((med.rad[,2] * pi)/med.irad[,2])))
names(reflec)<-c("wavelength", "reflectance")
reflec$reflectance[reflec$reflectance > 1] <- NA # change for non plotting

fm.night.reflec[[i]]<-reflec

} # night reflec
names(fm.night.reflec)<-index[,1]
sensor_wavelengths<-fm.night.reflec[[1]][,1] # important needed in final object!

rm(reflec, med.irad, med.rad, rad, rad.1, i.f, j,l, irad, irad.1)

fm.night.submerged<- vector("list", 7)
for (i in 1:dim(index)[1]){
  
  
  j<-index[i,2]
  l<-index[i,3]
  
  r.f<-file.r[j:l]
  is.f<-file.i[j:l]
  
  rad<-list()
  irad<-list()
  
  for (q in 1:length(r.f)){
    rad[[q]] <- read.csv(r.f[q])}
  for (k in 1:length(is.f)){
    irad[[k]] <- read.csv(is.f[k])}
  f.1<-function(x){x[,2][x[,2] < 0] <- NA
  return(x[28:118,1:2])}
  
  rad.1<-lapply(rad, f.1)
  irad.1<-lapply(irad, f.1)
  
  med.rad<-apply(laply(rad.1, as.matrix), c(2,3), median)
  med.irad<-apply(laply(irad.1, as.matrix), c(2,3), median)
  
  rownames(med.rad) <- c()
  rownames(med.irad) <- c()
  
  submerged<- as.data.frame(cbind(med.rad[,1], med.rad[,2], med.irad[,2]))
  names(submerged)<-c("wavelength", "radiance", "irradiance")

  
  fm.night.submerged[[i]]<-submerged
  
} # night rad surf
names(fm.night.submerged)<-index[,1]

fm.night.surf<- vector("list", 7)
for (i in 1:dim(index)[1]){
  
  
  j<-index[i,2]
  l<-index[i,3]
  
  r.f<-file.r[j:l]
  is.f<-file.i[j:l]
  
  rad<-list()
  irad<-list()
  
  for (q in 1:length(r.f)){
    rad[[q]] <- read.csv(r.f[q])}
  for (k in 1:length(is.f)){
    irad[[k]] <- read.csv(is.f[k])}
  f.1<-function(x){x[,2][x[,2] < 0] <- NA
  return(x[28:118,1:2])}
  
  rad.1<-lapply(rad, f.1)
  irad.1<-lapply(irad, f.1)
  
  med.rad<-apply(laply(rad.1, as.matrix), c(2,3), median)
  med.irad<-apply(laply(irad.1, as.matrix), c(2,3), median)
  
  rownames(med.rad) <- c()
  rownames(med.irad) <- c()
  
  surf<- as.data.frame(cbind(med.rad[,1], med.rad[,2], med.irad[,2]))
  names(surf)<-c("wavelength", "radiance", "irradiance")
  
  
  fm.night.surf[[i]]<-surf
  
} # night rad surf
names(fm.night.surf)<-index[,1]


### loops 2 - new moon #### 


nm.night.reflec<- vector("list", 3)

for (i in 1:dim(index)[1]){
  
  
  j<-index[i,2]
  l<-index[i,3]
  
  r.f<-nm.file.r[j:l]
  i.f<-nm.file.i[j:l]
  
  rad<-list()
  irad<-list()
  
  for (q in 1:length(r.f)){
    rad[[q]] <- read.csv(r.f[q])}
  for (k in 1:length(i.f)){
    irad[[k]] <- read.csv(i.f[k])}
  f.1<-function(x){x[,2][x[,2] < 0] <- NA
  return(x[28:118,1:2])}
  
  rad.1<-lapply(rad, f.1)
  irad.1<-lapply(irad, f.1)
  
  med.rad<-apply(laply(rad.1, as.matrix), c(2,3), median)
  med.irad<-apply(laply(irad.1, as.matrix), c(2,3), median)
  
  rownames(med.rad) <- c()
  rownames(med.irad) <- c()
  
  reflec<- as.data.frame(cbind(med.rad[,1], ((med.rad[,2] * pi)/med.irad[,2])))
  names(reflec)<-c("wavelength", "reflectance")
  reflec$reflectance[reflec$reflectance > 1] <- NA # change for non plotting
  
  nm.night.reflec[[i]]<-reflec
  
} # night reflec

names(nm.night.reflec)<-index[,1]

nm.night.submerged<- vector("list", 3)

for (i in 1:dim(index)[1]){
  
  
  j<-index[i,2]
  l<-index[i,3]
  
  r.f<-nm.file.r[j:l]
  is.f<-nm.file.i[j:l]
  
  rad<-list()
  irad<-list()
  
  for (q in 1:length(r.f)){
    rad[[q]] <- read.csv(r.f[q])}
  for (k in 1:length(is.f)){
    irad[[k]] <- read.csv(is.f[k])}
  f.1<-function(x){x[,2][x[,2] < 0] <- NA
  return(x[28:118,1:2])}
  
  rad.1<-lapply(rad, f.1)
  irad.1<-lapply(irad, f.1)
  
  med.rad<-apply(laply(rad.1, as.matrix), c(2,3), median)
  med.irad<-apply(laply(irad.1, as.matrix), c(2,3), median)
  
  rownames(med.rad) <- c()
  rownames(med.irad) <- c()
  
  submerged<- as.data.frame(cbind(med.rad[,1], med.rad[,2], med.irad[,2]))
  names(submerged)<-c("wavelength", "radiance", "irradiance")
  
  
  nm.night.submerged[[i]]<-submerged
  
} # night rad surf
names(nm.night.submerged)<-index[,1]

i<-3
nm.night.surf<- vector("list", 3)
for (i in 1:dim(index)[1]){
  
  
  j<-index[i,2]
  l<-index[i,3]
  
  r.f<-nm.file.r[j:l]
  is.f<-nm.file.i[j:l]
  
  rad<-list()
  irad<-list()
  
  for (q in 1:length(r.f)){
    rad[[q]] <- read.csv(r.f[q])}
  for (k in 1:length(is.f)){
    irad[[k]] <- read.csv(is.f[k])}
  f.1<-function(x){x[,2][x[,2] < 0] <- NA
  return(x[28:118,1:2])}
  
  rad.1<-lapply(rad, f.1)
  irad.1<-lapply(irad, f.1)
  
  med.rad<-apply(laply(rad.1, as.matrix), c(2,3), median)
  med.irad<-apply(laply(irad.1, as.matrix), c(2,3), median)
  
  rownames(med.rad) <- c()
  rownames(med.irad) <- c()
  
  surf<- as.data.frame(cbind(med.rad[,1], med.rad[,2], med.irad[,2]))
  names(surf)<-c("wavelength", "radiance", "irradiance")
  
  
  nm.night.surf[[i]]<-surf
  
} # night rad surf
names(nm.night.surf)<-index[,1]


rm(reflec, med.irad, med.rad, rad, rad.1, i.f, j,l)



mean.fm_reflec<-aaply(laply(fm.night.reflec, as.matrix), c(2, 3), mean, na.rm=T)
mean.fm_sub<-aaply(laply(fm.night.submerged, as.matrix), c(2, 3), mean, na.rm=T)

mean.nm_reflec<-aaply(laply(nm.night.reflec, as.matrix), c(2, 3), mean, na.rm=T)
mean.nm_sub<-aaply(laply(nm.night.submerged, as.matrix), c(2, 3), mean, na.rm=T)


mean.fm_surf<-aaply(laply(fm.night.surf, as.matrix), c(2, 3), mean, na.rm=T)
mean.nm_surf<-aaply(laply(nm.night.surf, as.matrix), c(2, 3), mean, na.rm=T)



### creating r objects ####

fig_1<-matrix(data=NA, nrow=91, ncol=7, dimnames=NULL)

fig_1[,1]<-sensor_wavelengths
fig_1[,2]<-mean.fm_reflec[,2]
fig_1[,3]<-mean.nm_reflec[,2]
fig_1[,4]<-mean.fm_surf[,2]
fig_1[,5]<-mean.nm_surf[,2]
fig_1[,6]<-mean.fm_surf[,3]
fig_1[,7]<-mean.nm_surf[,3]




fig_1<-as.data.frame(fig_1)
names(fig_1)<-c("wavelength", "f.reflec", "n.reflec", "f.surf.rad", "n.surf.rad", "f.surf.irad", "n.surf.irad")


saveRDS(fig_1, "fig_1.RDS")

### plotting ####
wav<-as.numeric(c("400", "425", "440",  "450", "460", "475", "500", "525", "550", "600", "650", "700" ))

p.night.relfec<-ggplot(data = test2,aes(y=reflectance ,x=wavelength)) +
  #geom_line(linetype = "dashed")+
  #geom_point()+
  geom_smooth(method = "loess", se = FALSE)+
  #stat_smooth(geom = 'area', method= 'loess', span = 1/4)+
  #geom_area(na.rm=T) +
  #ggtitle(paste0("Device: ", s.data$IDDevice," Time: ",s.data$DateTime)) +
  
  labs(y="Reflectance")+
  scale_x_discrete(name= "Wavelength (nm)", limits = wav) +
  theme_classic()



p.night.submerged<-ggplot(data = night.submerged,aes(y=reflectance ,x=wavelength)) +
  #geom_line(linetype = "dashed")+
  geom_point()+
  geom_smooth(method = "loess", se = FALSE)+
  #stat_smooth(geom = 'area', method= 'loess', span = 1/4)+
  #geom_area(na.rm=T) +
  #ggtitle(paste0("Device: ", s.data$IDDevice," Time: ",s.data$DateTime)) +
  
  labs(y="Reflectance")+
  scale_x_discrete(name= "Wavelength (nm)", limits = wav) +
  theme_classic()



# other stuff:
### check index on full moon
### apply Christainas code to new moon data ####


# fix here!

rDir<-"Z:\\Personal\\Brandon\\Dissertation\\tomales\\field_site_data\\ramses_data_tomales_bay\\20190502\\duplicate_dat"
dirL <- list.files(rDir, full.names = T, pattern = ".dat$", recursive = T)




reorgDat <- function(fileName, outDir){
  allDat <- readLines(fileName)
  specStart2 <- grep('^\\[Spectrum\\]',allDat)
  # spectrum end
  specEnd2 <- grep('\\[END\\] of \\[Spectrum\\]',allDat)
  
  
  for (i in 1:length(specStart2)){
    specStart <- specStart2[i] #i
    specEnd <- specEnd2[i] #i
    
    # read in all of data for particular sample
    # subset the full file
    subDat <- allDat[specStart:specEnd]
    
    ### Spec info indexing ###
    siStart <- 2 # find start of spectrum info data
    siEnd <-  grep('^\\[Attributes\\]',subDat) - 1
    si <- subDat[siStart: siEnd]
    
    ### Attributes Indexing ###
    attrStart <- grep('^\\[Attributes\\]',subDat) + 1 # find start of attribute info
    attrEnd <- grep('\\[END\\] of \\[Attributes\\]',subDat) - 1 # find end attribute info
    attr <- subDat[attrStart:attrEnd] # define attribute info subset data by start and end 
    
    ### Read in Spectral data ###
    ### SpectralProf/Data Indexing ###
    datStart <- grep('^\\[DATA\\]',subDat) + 2
    datEnd <- grep('\\[END\\] of \\[DATA\\]',subDat) -1
    specDat <- subDat[datStart:datEnd] %>%
      str_trim() %>%
      as.data.frame() %>%
      separate(.[1], c("wavelength", "intensity","error","status"), " ")
    
    ### Oranize ancilliary information and add to Spectral data ### 
    ancilAll <- c(si,attr) #combine ancillary information together (attributes and spec info)
    ancilCol <- str_trim(str_extract(as.character(unlist(ancilAll)), "^.*(?=( =))")) # creat columns list and remove any spaces with trim
    ancilVal <- str_trim(str_extract(as.character(unlist(ancilAll)), "(?<=(= )).*$")) # create values correspoding to colums
    names(ancilVal) <- ancilCol # assign column names to values 
    ancilDf <- data.frame(as.list(ancilVal)) %>% slice(rep(1:n(), each = nrow(specDat))) #create ancillary infromation datafram
    
    # organized dataframe bind spectral infromation with ancillary information
    orgDat <- cbind(specDat,ancilDf)
    
    rf <- paste0(orgDat$IDDevice[1],"_", orgDat$IDDataTypeSub1[1],"_",gsub(" |:","",orgDat$DateTime[1]),".csv")
    # new file name
    newFile <- paste0(outDir,"\\",rf)
    # write csv
    write_csv(orgDat, newFile)
    
    print(paste0(newFile,"is finished"))
    rm(orgDat)  
  }
  
}
lapply(dirL, reorgDat, outDir=newoutDir)

n.orgL <-  list.files(newoutDir, full.names = T, recursive = T, pattern = "SAM_8586_CALIBRATED.*csv$")

myDat <- n.orgL %>% 
  map_dfr(read_csv) %>%
  filter(IDDataTypeSub1 == 'CALIBRATED') %>%
  group_by(IDDevice,DateTime) %>%
  do(plots = ggplot(data = .,aes(y=intensity,x=wavelength)) +
       geom_line() +
       ggtitle(paste0("Device: ", .$IDDevice," Time: ",.$DateTime)) + 
       theme_classic())









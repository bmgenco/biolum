# exploratory script 1
#  for re-analysis
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

#windows
#outDir<-"Z:\\Personal\\Brandon\\working_directory\\output"
#setwd("Z:\\Personal\\Brandon\\working_directory")
outDir<-"/home/brandon/labserv/Personal/Brandon/working_directory/wd_tomales_2019/output/"
wd<-"/home/brandon/labserv/Personal/Brandon/working_directory/wd_tomales_2019"
setwd(wd)

### plot files ####

s_89<-readRDS("SAM_8589_CALIBRATED.RDS")
s_81<-readRDS("SAM_8581_CALIBRATED.RDS")
s_86<-readRDS("SAM_8586_CALIBRATED.RDS")

data<-read.csv("output/SAM_8586_CALIBRATED_2019-06-17005930.csv")
r.data<-read.csv("output/SAM_8581_RAW_2019-06-17005930.csv")

n.data<-data[(data$wavelength<=750),]
py<-n.data[which.max(n.data[,2]),1]

nr.data<-r.data[(r.data$wavelength<=750),]
pyr<-nr.data[which.max(nr.data[,2]),1]


s_86[256,3][[1]] #peak for SAM_8586 2019-06-1700:59:30 - 466 w

s_86[247,3][[1]] #UVA spike??? 326.5073 w

s_81[256,3][[1]]
s_89[256,3][[1]]


# changing plot axis:
testDir<-"Z:\\Personal\\Brandon\\working_directory\\temp"

orgL <-  list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8586_CALIBRATED.*csv$")


# 400-700 data[28:118,1]



### new ####

wav<-as.numeric(c("400", "425", "440",  "450", "460", "475", "500", "525", "550", "600", "650", "700" ))

myDat <- orgL %>% 
  map_dfr(read_csv) %>%
  filter(IDDataTypeSub1 == 'CALIBRATED') %>%
  group_by(IDDevice,DateTime) %>%
  do(plots = ggplot(data = .,aes(y=intensity,x=wavelength)) +
       geom_line() +
       ggtitle(paste0("Device: ", .$IDDevice," Time: ",.$DateTime)) +
       #labs(x = "Wavelength (nm)") +
       labs(y=expression("Radiance" ~ (W *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
       scale_x_discrete(name= "Wavelength (nm)", limits = wav) +
       theme_classic())

myDat[78:85,3][[1]]


s_86[248:366,3][[1]]

i<-340
data<-read.csv(orgL[i])

pr<-read.csv(file.is[i])
unique(pr$Pressure)

s.data<-data[28:118,]
s.data$intensity[s.data$intensity < 0] <- 0

p.4<-ggplot(s.data,aes(y=intensity,x=wavelength)) +
  geom_line(col="green") +
  labs(title="Full Moon D4 1 m" )+
  labs(y=expression("Radiance" ~ (W *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  scale_x_discrete(name= "Wavelength (nm)", limits = wav) +
  theme_minimal() 

  
  
  
  
  
  

# temp plotting  radince removing noise

s.data<-data[28:118,]
s.data$intensity[s.data$intensity < 0] <- 0
s.data$intensity[s.data$intensity < 0] <- NA


p<-ggplot(data = s.data,aes(y=intensity,x=wavelength)) +
  geom_line(na.rm=F) +
  ggtitle(paste0("Device: ", s.data$IDDevice," Time: ",s.data$DateTime)) +
  
  labs(y=expression("Radiance" ~ (W *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  scale_x_discrete(name= "Wavelength (nm)", limits = wav) +
  theme_classic()

gridExtra::grid.arrange(p.1,  p.2, p.3, p.4, ncol=2)

### pressure check ####

orgL <-  list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8581_CALIBRATED.*csv$")

IDDataCal

s1<-orgL[363:365]

tdat<-s1 %>%
  map_dfr(read_csv)
#my.data<-list()

for (i in 1:length(s1)){
  my.data[[i]] <- read.csv(s1[i])}


unique(tdat$Pressure)

f1<-function(x){x$intensity[x$intensity < 0] <- NA}

f1<-function(x){x[,2][x[,2] < 0] <- NA
return(x[,1:2])}

t2<-lapply(my.data, f1)
med.dat<-apply(laply(t2, as.matrix), c(2,3), median)



### water-leaving reflectance ####

file.r <-  list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8586_CALIBRATED.*csv$")
file.i<-  list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8589_CALIBRATED.*csv$")

#spectra of water-leaving reflectance = pi * water leaving radiance/ downwelling irradince


# fix index out side function
index<-read.csv("june2019_sampling_index.csv")
index.n<-droplevels(index[c(1:4,7,8,10),c(1,8)]) # night stations
index.n$night.above.water<-as.character(index.n$night.above.water)
index.n$station<-as.character(index.n$station)

index<-separate(index.n, "night.above.water", paste("night.above.water", 1:2, sep=" "), sep=":", extra="drop")
index[,2]<-as.numeric(index[,2])
index[,3]<-as.numeric(index[,3])


# start here

i<-1

j<-index[i,2]
l<-index[i,3]

r.f<-file.r[j:l]
i.f<-file.i[j:l]

rad<-list()
irad<-list()

for (i in 1:length(r.f)){
  rad[[i]] <- read.csv(r.f[i])}
for (i in 1:length(i.f)){
  irad[[i]] <- read.csv(i.f[i])}
f.1<-function(x){x[,2][x[,2] < 0] <- NA
return(x[28:118,1:2])}

rad.1<-lapply(rad, f.1)
irad.1<-lapply(irad, f.1)

med.rad<-apply(laply(rad.1, as.matrix), c(2,3), meadian)
med.irad<-apply(laply(irad.1, as.matrix), c(2,3), median)

rownames(med.rad) <- c()
rownames(med.irad) <- c()

reflec<- as.data.frame(cbind(med.rad[,1], ((med.rad[,2] * pi)/med.irad[,2])))
names(reflec)<-c("wavelength", "reflectance")
reflec$reflectance[reflec$reflectance > 1] <- NA

wav<-as.numeric(c("400", "425", "440",  "450", "460", "475", "500", "525", "550", "600", "650", "700" ))

#spline.r<-as.data.frame(spline(reflec$wavelength, reflec$reflectance))

p<-ggplot(data = reflec,aes(y=reflectance ,x=wavelength)) +
  #geom_line(linetype = "dashed")+
  geom_point()+
  geom_smooth(method = "loess", se = FALSE)+
  #stat_smooth(geom = 'area', method= 'loess', span = 1/4)+
  #geom_area(na.rm=T) +
  #ggtitle(paste0("Device: ", s.data$IDDevice," Time: ",s.data$DateTime)) +
  
  labs(y="Reflectance")+
  scale_x_discrete(name= "Wavelength (nm)", limits = wav) +
  theme_classic()



### end

ir.w<-read.csv("Z:\\Personal\\Brandon\\working_directory\\output\\SAM_8581_CALIBRATED_2019-06-17005930.csv")
ir.a<-read.csv("Z:\\Personal\\Brandon\\working_directory\\output\\SAM_8589_CALIBRATED_2019-06-17005930.csv")



test<-file.r[index.n[1,c(2)]]


x$intensity[x$intensity < 0] <- NA
x$intesity<-as.numeric(x$intesity)



intensity<-(ir.a$intensity/ir.w$intensity)
test2<-cbind(ir.a$wavelength, intensity)
as.numeric(test2[,1])
as.numeric(test2[,2])
test2<-as.data.frame(test2)
colnames(test2)<-c("wavelength", "reflectance")
test2$reflectance[is.nan(test2$reflectance)] <- NA
test2<-as.data.frame(test2)



f.wr<-function(x){}





### plots


p.irw<-ggplot(data = ir.w,aes(y=intensity,x=wavelength)) +
  geom_line(na.rm=F) +
  ggtitle(paste0("Device: ", ir.w$IDDevice," Time: ",ir.w$DateTime))

p.ira<-ggplot(data = ir.a,aes(y=intensity,x=wavelength)) +
  geom_line(na.rm=F) +
  ggtitle(paste0("Device: ", ir.a$IDDevice," Time: ",ir.a$DateTime))


r<-ggplot(data = test2,aes(y=reflectance,x=wavelength)) +
  geom_line(na.rm=T)
  #ggtitle(paste0("Device: ", ir.a$IDDevice," Time: ",ir.a$DateTime)

#### old ####
myDat <- orgL %>% 
  map_dfr(read_csv) %>%
  filter(IDDataTypeSub1 == 'CALIBRATED') %>%
  group_by(IDDevice,DateTime) %>%
  do(plots = ggplot(data = .,aes(y=intensity,x=wavelength)) +
       geom_line() +
       ggtitle(paste0("Device: ", .$IDDevice," Time: ",.$DateTime)) + 
       theme_classic())


### notes: ####
# ideas:
# https://stackoverflow.com/questions/39456942/aggregate-data-frame-with-list-column
# https://stackoverflow.com/questions/21982987/mean-per-group-in-a-data-frame



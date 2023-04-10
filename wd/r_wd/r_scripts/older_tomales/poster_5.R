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

outDir<-"Z:\\Personal\\Brandon\\working_directory\\output"
setwd("Z:\\Personal\\Brandon\\working_directory")
newoutDir<-"Z:\\Personal\\Brandon\\working_directory\\new_moon_output"

### load files for for loops ####

file.r <-list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8586_CALIBRATED.*csv$")
file.i<-list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8589_CALIBRATED.*csv$")
file.is<-list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8581_CALIBRATED.*csv$")


nm.file.r <-list.files(newoutDir, full.names = T, recursive = T, pattern = "SAM_8586_CALIBRATED.*csv$")
nm.file.i<-list.files(newoutDir, full.names = T, recursive = T, pattern = "SAM_8589_CALIBRATED.*csv$")
nm.file.is<-list.files(newoutDir, full.names = T, recursive = T, pattern = "SAM_8581_CALIBRATED.*csv$")


shit[[1]]<-sub.d1
shit[[2]]<-sub.d2
shit[[3]]<-sub.d4
shit[[4]]<-sub.d5
shit[[5]]<-sub.s1
shit[[6]]<-sub.s2
shit[[7]]<-sub.b1  
  
  # top meter


## new loop:

top.fm.night.submerged<- vector("list", 7)
i<-7
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
  
  nm.x<
  
  irad.1<-lapply(irad, f.1)
  
  #rownames(med.rad) <- c()
  #rownames(med.irad) <- c()
  
  submerged<- as.data.frame(cbind(rad.1[,1], rad.1[,2], irad.[,2]))
  names(submerged)<-c("wavelength", "radiance", "irradiance")
  
  
  top.fm.night.submerged[[i]]<-submerged}
names(top.fm.night.submerged)<-index[,1]


d1p<-file.is[248:254]
d2p<-file.is[287:293]


s1<-d2p
f.1<-function(x){x<-x[1:6]}

t1<-lapply(shit, f.1)

f.2<-function(x){apply(laply(x, as.matrix), c(2,3), median)}
t2<-lapply(t1, f.2)
names(t2)<-index[,1]



plot_names<-c("Station D1","Station D2","Station D4","Station D5", "Station S1","Station S2","Station B1")


wav<-as.numeric(c("400", "425", "440",  "450", "460", "475", "500", "525", "550", "600", "650", "700" ))


i<-1
df<-as.data.frame(t2[[i]])
pl.1<- ggplot(df, aes(x=wavelength)) +
  labs(title="Figure 2: Full Moon 0-5 meter averaged Upwelling Radiance")+

  labs(subtitle=plot_names[i])+
  geom_smooth(aes(y=intensity), method="loess", col="green", se=FALSE) +
  geom_point(aes(y=intensity), col="black")+
  #geom_line(aes(y=intensity))+
  labs(y=expression("Upwelling Radiance" ~ ( mW *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  #scale_x_discrete(limits = wav)+
  theme_minimal() + 
  #theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

tiff("figure_two.tiff", width=8.5, units="in",  res=1200)
gridExtra::grid.arrange(pl.4,  pl.1, pl.6, pl.7, ncol=2)
dev.off()

unique(tdat$Pressure)



s1 <- orgL %>% 
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

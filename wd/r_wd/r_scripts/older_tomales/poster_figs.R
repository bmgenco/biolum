#poster 4

# update for linux 2019.12.12

#figs

# https://felixfan.github.io/stacking-plots-same-x/

rm(list=ls())

f.ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("plyr", "tidyverse", "tibble", "stringr", "tools", "ggplot2","devtools", "patchwork") 
f.ipak(packages) 
rm(packages)


## require packages
#require(plyr)
#require(tidyverse)
#require(tibble)
#require(stringr)
#require(tools)
#require(ggplot2)
#require(devtools)
#require(patchwork)

#outDir<-"/home/brandon/labserv/Personal/Brandon/working_directory/output/"
# modifcation for FINEEST

outDir<-"/home/brandon/labserv/Personal/Brandon/working_directory/wd_tomales_2019/output/"

wd<-"/home/brandon/labserv/Personal/Brandon/working_directory/wd_tomales_2019"
setwd(wd)

# radiance units
#   labs(y=expression("Radiance" ~ (W *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
#     scale_x_discrete(name= "Wavelength (nm)", limits = wav) +
#     theme_classic())


#irradince uits
#   labs(y=expression("Radiance" ~ (W *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
#     scale_x_discrete(name= "Wavelength (nm)", limits = wav) +
#     theme_classic())

 

fig_1<-readRDS("fig_1.RDS")
wav<-as.numeric(c("400", "425", "440",  "450", "460", "475", "500", "525", "550", "600", "650", "700" ))
#wav<-as.numeric(c("400", "425", "450",  "40", "460", "475", "500", "525", "550", "600", "650", "700" ))

### New Figure One:


# fix scale

fig_1[,4]<-fig_1[,4]*1000
fig_1[,5]<-fig_1[,5]*1000
fig_1[,6]<-fig_1[,6]*1000
fig_1[,7]<-fig_1[,7]*1000

p2 <- ggplot(fig_1, aes(x=wavelength)) +       
  geom_smooth(aes(y=n.surf.rad), method="loess", col="green", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=f.surf.rad), method="loess", col="green", se=FALSE) +
  labs(y=expression("Upwelling Radiance" ~ ( mW *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  scale_x_discrete(limits = wav)+
  theme_minimal() + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

png("fig_two _updated_finesst", height=8, width=10, units="in", res=1200)
p2
dev.off()

# fix scale

fig_1[,4]<-fig_1[,4]*1000
fig_1[,5]<-fig_1[,5]*1000
fig_1[,6]<-fig_1[,6]*1000
fig_1[,7]<-fig_1[,7]*1000


### multipanel plots   ####

# fig one - 3 panel ####

#windowsFonts(Times=windowsFont("TT Times New Roman")) # windows code only

p1<-ggplot(fig_1, aes(x=wavelength)) +
     labs(title="Figure 1: Irradiance - Radiance - Reflectance for New and Full Moon Sampling Periods",
          subtitle = "New Moon = Dashed Line    Full Moon= Solid Line" )+
  
      geom_smooth(aes(y=n.surf.irad), method="loess", col="magenta", se=FALSE, linetype = "dashed") +
      geom_smooth(aes(y=f.surf.irad ), method="loess", col="magenta", se=FALSE) +
      labs(y=expression("Downwelling Irradiance" ~ (mW *  "\U00B7"  *  m^{-2})))+
      scale_x_discrete( limits = wav)+
      theme_minimal() + 
      theme(axis.title.x = element_blank())+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.subtitle = element_text(hjust = 0.5))+
      theme(plot.margin = unit(c(1,1,1,.9), "cm"))

p2 <- ggplot(fig_1, aes(x=wavelength)) +       
  geom_smooth(aes(y=n.surf.rad), method="loess", col="green", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=f.surf.rad), method="loess", col="green", se=FALSE) +
  labs(y=expression("Upwelling Radiance" ~ ( mW *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  scale_x_discrete(limits = wav)+
  theme_minimal() + 
  theme(axis.title.x = element_blank())+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))


p3<-ggplot(fig_1, aes(x=wavelength))+
  geom_smooth(aes(y=n.reflec), method="loess", col="black", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=f.reflec ), method="loess", col="black", se=FALSE ) +
  labs(y=expression("Reflectance" ~ (0 - 1)))+
  scale_x_discrete(name= "Wavelength (nm)", limits = wav)+
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1.1), "cm"))


png("fig one", height=10.5, width=8, units="in", res=1200)
gridExtra::grid.arrange(p1, p2,p3)
dev.off()

# fig one - 2 panel ####
p1<-ggplot(fig_1, aes(x=wavelength)) +

  labs(title="Figure 1: Irradiance - Radiance for New and Full Moon Sampling",
     subtitle = "New Moon = Dashed Line    Full Moon= Solid Line" )+
  
  geom_smooth(aes(y=n.surf.irad), method="loess", col="magenta", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=f.surf.irad ), method="loess", col="magenta", se=FALSE) +
  labs(y=expression("Downwelling Irradiance" ~ (mW *  "\U00B7"  *  m^{-2})))+
  scale_x_discrete( limits = wav)+
  theme_minimal() + 
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1,1,1,.9), "cm"))

p2 <- ggplot(fig_1, aes(x=wavelength)) +       
  geom_smooth(aes(y=n.surf.rad), method="loess", col="green", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=f.surf.rad), method="loess", col="green", se=FALSE) +
  labs(y=expression("Upwelling Radiance" ~ ( mW *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  scale_x_discrete(name= "Wavelength (nm)", limits = wav)+
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1.1), "cm"))
  


ggsave (gridExtra::grid.arrange(p1, p2), filename="fig_one_2_panel.png", dpi=1200, type ="cairo")









# figure 2####
i<-3
df<-rad.1[[i]]
p.1<-ggplot(df, aes(x=wavelength)) +       
  geom_smooth(aes(y=intensity), method="loess", col="green", se=FALSE, linetype = "dashed") +
  #geom_smooth(aes(y=f.surf.rad), method="loess", col="green", se=FALSE) +
  labs(y=expression("Upwelling Radiance" ~ ( mW *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  scale_x_discrete(limits = wav)+
  theme_minimal() + 
  theme(axis.title.x = element_blank())+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

p.1


getwd()

nm.night.submerged<-readRDS("nm.night.submerged.RDS")
nm.night.surf<-readRDS("nm.night.surf.RDS")

mean.fm_sub<-as.data.frame(mean.fm_sub)

p.fm.sub<-ggplot(mean.fm_sub, aes(x=wavelength))+
  geom_smooth(aes(y=radiance), method="loess", col="green", se=FALSE, linetype = "dashed") +
  #geom_smooth(aes(y=f.surf.rad), method="loess", col="green", se=FALSE) +
  labs(y=expression("Upwelling Radiance" ~ ( mW *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  scale_x_discrete( limits = wav)+
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1.1), "cm"))




s1<-ggplot(nm.night.surf[[3]], aes(x=wavelength))+
  geom_smooth(aes(y=radiance), method="loess", col="green", se=FALSE, linetype = "dashed") +
  #geom_smooth(aes(y=f.surf.rad), method="loess", col="green", se=FALSE) +
  labs(y=expression("Upwelling Radiance" ~ ( mW *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  scale_x_discrete( limits = wav)+
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1.1), "cm"))


s.s1<-ggplot(nm.night.submerged[[3]], aes(x=wavelength))+
  geom_smooth(aes(y=radiance), method="loess", col="green", se=FALSE, linetype = "dashed") +
  #geom_smooth(aes(y=f.surf.rad), method="loess", col="green", se=FALSE) +
  labs(y=expression("Upwelling Radiance" ~ ( mW *  "\U00B7" * sr^{-1} * "\U00B7" *  m^{-2})))+
  scale_x_discrete( limits = wav)+
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1.1), "cm"))



  gridExtra::grid.arrange(s.d1,s.d2, s.s1)

  ### Multi y axis plots... do not use ####


m4<-max(fig_1[,4], na.rm = T)
m5<-max(fig_1[,5], na.rm = T)
m6<-max(fig_1[,6], na.rm = T)
m7<-max(fig_1[,7], na.rm = T)


  
ly<- #rad
ry<- #irad
  
scaleFactor <- m5 / m7

p.fig_3<-ggplot(fig_1, aes(x=wavelength)) +
  #geom_smooth(aes(y=n.surf.rad), method="loess", col="green", se=FALSE) +
  geom_smooth(aes(y=f.surf.rad), method="loess", col="green", se=FALSE, linetype = "dashed") +
  
  geom_smooth(aes(y=n.surf.irad * scaleFactor), method="loess", col="magenta", se=FALSE) +
  #geom_smooth(aes(y=f.surf.irad * scaleFactor), method="loess", col="magenta", se=FALSE, linetype = "dashed") +
  
  
  scale_y_continuous(name="radiance", sec.axis=sec_axis(~./scaleFactor, name="irrad")) +
  theme(
    axis.title.y.left=element_text(color="green"),
    axis.text.y.left=element_text(color="green"),
    axis.title.y.right=element_text(color="magenta"),
    axis.text.y.right=element_text(color="magenta")
  )
p.fig_3

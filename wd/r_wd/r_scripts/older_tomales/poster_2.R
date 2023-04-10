
wd<-"/home/brandon/labserv/Personal/Brandon/working_directory/wd_tomales_2019"
setwd(wd)


wdh<-"C:\\Users\\bgenco\\Dropbox\\temp_R_drop\\science\\dissertation\\wd_poster_temp"
wd<-"/home/brandon/Dropbox/temp_R_drop/science/dissertation/wd_poster_temp"


setwd(wdh)


t2<-readRDS("t2.RDS")
head(t2)
dim(t2)


require(plyr)
# ideas:
# https://stackoverflow.com/questions/39456942/aggregate-data-frame-with-list-column
# https://stackoverflow.com/questions/21982987/mean-per-group-in-a-data-frame



med.dat<-apply(laply(t2, as.matrix), c(2,3), median)

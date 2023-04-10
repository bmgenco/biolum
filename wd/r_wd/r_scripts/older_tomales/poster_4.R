## check index new moon


17+45

s1<-nm.file.is[80:84]

tdat<-s1 %>%
  map_dfr(read_csv)
#my.data<-list()

for (i in 1:length(s1)){
  my.data[[i]] <- read.csv(s1[i])}

unique(tdat$Pressure)

tdat[1]$Pressure
dim(tdat)

ch<-nm.file.is[81]
check<-ch %>%
  map_dfr(read_csv)
unique(check$Pressure)


orgL <-  list.files(newoutDir, full.names = T, recursive = T, pattern = "SAM_8586_CALIBRATED.*csv$")

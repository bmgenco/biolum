
#poster # calibration check

# test calibration


wd<-"Z:\\Personal\\Brandon\\working_directory\\20190716_trios_calibration\\"

setwd(wd)

dirL <- list.files(wd, full.names = T, pattern = ".dat$", recursive = T)
outDir<-"Z:\\Personal\\Brandon\\working_directory\\20190716_trios_calibration\\output"
fileName <- dirL[1]

##################### PART 1 : Reorganize data #####################################
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

# apply to all data
lapply(dirL, reorgDat, outDir= outDir)
### plots

orgL <-  list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8586_CALIBRATED.*csv$")

myDat <- orgL %>% 
  map_dfr(read_csv) %>%
  filter(IDDataTypeSub1 == 'CALIBRATED') %>%
  group_by(IDDevice,DateTime) %>%
  do(plots = ggplot(data = .,aes(y=intensity,x=wavelength)) +
       geom_line() +
       ggtitle(paste0("Device: ", .$IDDevice," Time: ",.$DateTime)) + 
       theme_classic())
### check calibration file

data1<-read.csv("output/SAM_8589_CALIBRATED_2019-07-16173447.csv")
data2<-read.csv("output/SAM_8589_CALIBRATED_2019-07-16173457.csv")




data3<-read.csv("output/SAM_8581_CALIBRATED_2019-07-16132731.csv")
data4<-read.csv("output/SAM_8581_CALIBRATED_2019-07-16132812.csv")


data5<-read.csv("output/SAM_8586_CALIBRATED_2019-07-16172908.csv")
data6<-read.csv("output/SAM_8586_CALIBRATED_2019-07-16172933.csv")




unique(data1$IDDataCal)
unique(data2$IDDataCal)
unique(data3$IDDataCal)
unique(data4$IDDataCal)

test<-sqrt((data6[,2])^2)-sqrt((data5[,2])^2)

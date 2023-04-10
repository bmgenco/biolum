# Title: Creating read capability for Ramses data
# Status: pending
# Author: Christiana Ade
# Date: April 22
# Modified: June 14th
# Purpose: read ramses data from brandons project that we actually took in the bay
# **Requires** 
# 1) ramses .dat file and matching field data
####################################################################################


rm(list=ls())




## require packages
require(tidyverse)
require(tibble)
require(lubridate)
require(stringr)
require(tools)
require(ggplot2)
require(devtools)
require(patchwork) # devtools::install_github("thomasp85/patchwork")
# set working directory
#setwd("Z:\\Cade\\Test\\TestRamseBrandon")

#setwd("C:\\Users\\brandon\\Desktop\\tomales june 2019")

setwd("Z:\\Personal\\Brandon\\working_directory")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
# 1) Directory where all the Ramses data live
#rDir <- "./DataLogTribox95A7"

#rDir<-"C:\\Users\\brandon\\Desktop\\tomales june 2019\\data\\ramses_split\\Export of Tribox_95A7 on 2019-06-18 13-47-46"
#rDir<-"C:\\Users\\brandon\\Desktop\\tomales june 2019\\data\\ramses_unsplit\\Export of Tribox_95A7 on 2019-06-18 13-45-42"

rDir<-"Z:\\Personal\\Brandon\\Dissertation\\tomales\\field_site_data\\ramses_data_tomales_bay\\201906"


# 2) List of all Ramses Files
dirL <- list.files(rDir, full.names = T, pattern = ".dat$", recursive = T)
#### OUTPUT FILES ####
#outDir <- "Z:\\Cade\\Test\\TestRamseBrandon\\plots"
outDir<-"Z:\\Personal\\Brandon\\working_directory\\output"

#### USER DEFINED FUNCTIONS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

##################### PART 2: Plot data #####################################

### Organized data read in 
orgL <-  list.files(outDir, full.names = T, recursive = T, pattern = "SAM_8581_CALIBRATED.*csv$")

myDat <- orgL %>% 
  map_dfr(read_csv) %>%
  filter(IDDataTypeSub1 == 'CALIBRATED') %>%
  group_by(IDDevice,DateTime) %>%
  do(plots = ggplot(data = .,aes(y=intensity,x=wavelength)) +
       geom_line() +
       ggtitle(paste0("Device: ", .$IDDevice," Time: ",.$DateTime)) + 
       theme_classic())

# find unique time
# note that "./DataLogTribox95A7/SAMIP_50DB/SAMIP_50DB_2019-04-05_organized.csv" has two extra times 
# investigate why this is -c(1,2) removes these times
uniTime <- unique(myDat$DateTime) [-c(1,2)]

#### why isn't this working for uni in uniTime??
# bg - chnage belwo
for (uni in 1:length(uniTime)){
  mPlots <- myDat %>%
    filter(DateTime == uniTime[uni])
  
  finalPlot <- mPlots$plots[[1]] + mPlots$plots[[2]] + mPlots$plots[[3]] + plot_layout(ncol = 2, widths = c(1, 1))
  finalPlot
  #fileName
  datTime <- gsub(" ","_",uniTime[uni])
  datTime <- gsub(":","_",datTime)
  fn <- paste0(outDir,"\\plot_", datTime,".png")
  ggsave(fn)
  
}


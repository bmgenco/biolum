# Title: Creating read capability for Ramses data
# Status: pending
# Author: Christiana Ade
# Date: April 22
# Modified: 
# Purpose: read ramses data from brandons project that we actually took in the bay
# **Requires** 
# 1) ramses .dat file and matching field data
####################################################################################

rm(list=ls())

## require packages
require(tidyverse)
require(lubridate)
require(stringr)
require(tools)
# set working directory

#setwd("Z:\\Cade\\Test\\TestRamseBrandon")
setwd("Z:\\Personal\\Brandon\\working_directory\\TestRamseBrandon")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
# 1) Directory where all the Ramses data live
rDir <- "./DataLogTribox95A7"
# 2) List of all Ramses Files
dirL <- list.files(rDir, full.names = T, recursive = T)
#### OUTPUT FILES ####
#### USER DEFINED FUNCTIONS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rtF <- dirL[6]

for (i in length(rtF)){
  # dir
  
  # readlines
  rtDat <- readLines(rtF)
  specStart2 <- grep('^\\[Spectrum\\]',rtDat)
  # spectrum end
  specEnd2 <- grep('\\[END\\] of \\[Spectrum\\]',rtDat)
  augDat <-NULL
  
  for (i in 1:length(specStart2)){
    specStart <- specStart2[i] #i
    specEnd <- specEnd2[i] #i
    
    # read in all of data for particular sample
    dat1 <- rtDat[specStart:specEnd]
    ### Attributes Indexing ###
    atStart <- grep('^\\[Attributes\\]',dat1)
    atEnd <- grep('\\[END\\] of \\[Attributes\\]',dat1)
    ### Data Indexing ###
    datStart <- grep('^\\[DATA\\]',dat1)
    datEnd <- grep('\\[END\\] of \\[DATA\\]',dat1)
    
    ### Subset Data by Information ###
    # read in ancillary data (specdata)
    ancil <- rtDat[(specStart + 1):(atStart - 1)]
    # read in attribute data
    attr <- rtDat[(atStart + 1):(atEnd - 1)]
    
    ### Read in Spectral data ###
  
    
    dat2<-dat1[(datStart + 2):(datEnd - 1)]
    b<-matrix( NA, nrow=length(dat2), ncol=4)
    for (i in 1:length(dat2)){
      t3<-strsplit(dat2[i], split = " ", fixed=F)
      x<-as.numeric(t3[[1]][2:5])
      b[i,]=x}
    
   names(b)<-c("wavelength", "intensity","error","status")
   trioSpec<-b
   rm(b, dat2, t3)
    
   #trioSpec <- suppressWarnings(read_delim(rtF, skip = (datStart + ), n_max = (datEnd - 1), col_names = F, delim = " ") %>% select(.,2,3,4,5) %>% setNames(c("wavelength", "intensity","error","status")))
    
    ### Oranize ancilliary information and add to Spectral data ### 
    ancilAll <- c(ancil,attr)
    ancilCol <- str_trim(str_extract(as.character(unlist(ancilAll)), "^.*(?=( =))"))
    ancilVal <- str_trim(str_extract(as.character(unlist(ancilAll)), "(?<=(= )).*$"))
    names(ancilVal) <- ancilCol
    atm <- data.frame(as.list(ancilVal)) %>% slice(rep(1:n(), each = nrow(trioSpec)))
    
    # organized dataframe 
    orgDat <- bind_cols(trioSpec,atm)
    
    # append new dataset
    augDat <- bind_rows(augDat,orgDat)
  }
  
  m <- (rtF)
    
}






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Spectrum Indexing ###
# # spectrum start
# specStart <- grep('^\\[Spectrum\\]',rtDat)
# # spectrum end
# specEnd <- grep('\\[END\\] of \\[Spectrum\\]',rtDat)
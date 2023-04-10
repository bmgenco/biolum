######################### Tomales Reanalysis 2021 #########################
#                                 2021-11-4                               #
#                         Plan for scripts:                               #
#     (1) .Dat files to CSV - RAW and  default Calibrated options         #
#     (2) .csv -> r object                                                #
#         A: calibration options                                          #
#         B: Combine by station, date, instrument depth etc               #
#         C: Output R object -portable  r object (?)                      #
#     (3) Analysis script/ step                                           #
#         A: smoothing                                                    #                    
#         B: flexible                                                     #
#     (4) Plotting (read in new r objects?)                               #
#                                                                         #
#########################     computer: vesta     #########################


#### setup ####
rm(list=ls())

wd<-"/home/brandon/vestawd/biolum/wd/r_wd" # fro r script

# setwd("..") # for rmarkdown
# wd<-getwd() # for rmarkdown

robj<-"./r_objects"
fig<-"../../figures"
ramses<-"../../data/ramses"
calibration_d<-"../../data/ramses/from_manufacturer/calibration"
data_d<-"../../data"
sonde<-"../../data/sonde"
gis_data<-"../../data/gis"


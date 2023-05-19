# Author: Christiana Ade
# Date: 4/6/2019
# Modified: 
# Purpose: Check the plot with the rameses video to see if the LABELED plot is the same as 
# the one recorded
# **Requires** 
# 1)  
####################################################################################
## require packages
require(tidyverse)
require(ggplot2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
dat <- read_csv("Z:\\Cade\\Test\\TestRamseBrandon\\testPlot.csv")
#### OUTPUT FILES ####
#### USER DEFINED FUNCTIONS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p <- ggplot(data = dat, aes(y= intensity, x= wavelength)) +
  geom_line()

p
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
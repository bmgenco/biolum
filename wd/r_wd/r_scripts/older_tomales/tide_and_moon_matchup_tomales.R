


rm(list=ls())
data<-"/home/brandon/labserv/Personal/Brandon/Dissertation/external_data/tomales"
wd<-"/home/brandon/labserv/Personal/Brandon/working_directory/"
outDir<-"/home/brandon/labserv/Personal/Brandon/working_directory/master_output/"




f.ipak <- function(pkg){
  
  # loads packages, quietly, given by a vector of package names e.g., pkg<-c("ggplot", "tidyverse")
  # will install  packages listed , and their dependencies, if needed.
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, quiet=T, verbose = F)
  sapply(pkg, require, character.only = TRUE, quietly = FALSE, warn.conflicts=F)
}
packages<-c("tidyverse")  # set packages here
f.ipak(packages)
#rm(f.ipak, packages, wd)

ent<-read.delim2()



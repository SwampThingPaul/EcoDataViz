## 
## Eco #DataViz (7/9/2019)
##
## Code was compiled by Paul Julian
## contact info: pjulian@ufl.edu

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()


#Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")
library(plyr)
library(reshape)

#Paths
#setwd("D:/_GitHub/PseudoTidyTuesday")
wd="D:/_GitHub/PseudoTidyTuesday"

#paths=paste0(getwd(),c("/Plots/","/Data/"))
paths=paste0(wd,c("/Plots/","/Data/20190709/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
data.path=paths[2];


# Data --------------------------------------------------------------------
# Downloaded Pan-Arctic Ice Ocean Modeling and Assimilation System data from
## http://psc.apl.uw.edu/research/projects/arctic-sea-ice-volume-anomaly/data/ 
## and unzipped the data (need to find a more eligent R-based approach).

thick.dat=read.table(paste0(data.path,"PIOMAS.thick.daily.1979.2019.Current.v2.1.dat"),header=F,skip=1,col.names=c("Year","Day","Thickness_m"))
vol.dat=read.table(paste0(data.path,"PIOMAS.vol.daily.1979.2019.Current.v2.1.dat"),header=F,skip=1,col.names=c("Year","Day","Vol_km3"))
vol.dat$Vol_km3=vol.dat$Vol_km3*1E+3

dat=merge(thick.dat,vol.dat,c("Year","Day"))

plot(Thickness_m~Vol_km3,dat)
plot(Thickness_m~Vol_km3,subset(dat,Year==1979),ylim=c(1,3),xlim=c(2500,50000))
with(subset(dat,Year==2018),points(Vol_km3,Thickness_m))

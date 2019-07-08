## 
## Eco #DataViz (7/2/2019)
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
library(RColorBrewer)
library(classInt)

#Paths
#setwd("D:/_GitHub/PseudoTidyTuesday")
wd="D:/_GitHub/PseudoTidyTuesday"

#paths=paste0(getwd(),c("/Plots/","/Data/"))
paths=paste0(wd,c("/Plots/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
#data.path=paths[2];



# Data --------------------------------------------------------------------
# Rainfall data f rom the SLEE rainfall
# site in SW Florida, along the Calooshatchee River
dates=as.Date(c("1970-01-01","2019-01-01"))

dat=DBHYDRO_daily(dates[1],dates[2],"06081")
plot(Data.Value~Date,dat);#sanity check

#Convert date to month and years
dat$month=as.numeric(format(dat$Date,"%m"))
dat$CY=as.numeric(format(dat$Date,"%Y"))

#convert standard to metric measurement
dat$RF.cm=in.to.cm(dat$Data.Value)

#Check for NAs
sum(is.na(dat$RF.cm))

#assume NAs = no data
dat$RF.cm[is.na(dat$RF.cm)]=0
plot(RF.cm~Date,dat);#sanity check

## Aggregate and cross tabulate data by month and year
dat.xtab=cast(subset(dat,CY!=2019),month~CY,value="RF.cm",sum)

dat.xtab.m=as.matrix(dat.xtab)[12:1,]
dat.xtab.m[1:5, 1:5]

cols=colorRampPalette(c("indianred1","dodgerblue1"))(50)
heatmap(dat.xtab.m, Colv = NA, Rowv = NA, 
        scale = "none", 
        col = cols,
        xlab = "Year",
        ylab = "Month",
        margins = c(4, 4))

library(ggthemes)
library(ggplot2)

dat.agg=ddply(subset(dat,CY!=2019),c("month","CY"),summarise,TRF=sum(RF.cm,na.rm=T))
ggplot(data=dat.agg,aes(x=CY,y=month))+
  scale_y_continuous(breaks=c(1:12), expand=c(0,0))+
  geom_tile(aes(fill=TRF))+
  scale_fill_viridis_c(option="magma") + theme_tufte(base_family="Helvetica")


cols=colorRampPalette(c("cornsilk","dodgerblue1"))(12)
library(ggplot2)
ggplot(dat.agg, aes(CY, month)) +
  geom_tile(aes(fill = TRF)) +
  scale_fill_gradientn(colors = cols) +
  scale_y_discrete(limits = rev(unique(dat.agg$month))) +
  ggtitle("Rainfall along the Caloosahathcee River") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

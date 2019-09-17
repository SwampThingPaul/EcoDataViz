## 
## Eco #DataViz (09/17/2019)
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
wd="D:/_GitHub/EcoDataViz"

#paths=paste0(getwd(),c("/Plots/","/Data/"))
paths=paste0(wd,c("/Plots/","/Data/20190625/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
data.path=paths[2];


# Data --------------------------------------------------------------------
# https://nsidc.org/arcticseaicenews/charctic-interactive-sea-ice-graph/
#Anartic Data
#dat=read.csv("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/daily/data/S_seaice_extent_daily_v3.0.csv",skip=2,
#             col.names=c("Year","Month","Day","Extent.10e6km2","Missing.10e6km2","Source"))

# Artic Data
dat=read.csv("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/N_seaice_extent_daily_v3.0.csv",skip=2,
             col.names=c("Year","Month","Day","Extent.10e6km2","Missing.10e6km2","Source"))
dat$Date=with(dat,date.fun(paste(Year,Month,Day,sep="-"),tz="GMT"))
dat$DOY=format(dat$Date,"%j")
POR.stats=ddply(subset(dat,Year%in%seq(1981,2010,1)),"DOY",summarise,
                N.val=N(Extent.10e6km2),
                decile.10=quantile(Extent.10e6km2,probs=0.1,na.rm=T),
                decile.90=quantile(Extent.10e6km2,probs=0.9,na.rm=T),
                Q25=quantile(Extent.10e6km2,probs=0.25,na.rm=T),
                Q75=quantile(Extent.10e6km2,probs=0.75,na.rm=T),
                median=median(Extent.10e6km2,na.rm=T))


# Plot --------------------------------------------------------------------
# Replicate https://nsidc.org/arcticseaicenews/charctic-interactive-sea-ice-graph/

ylim.val=c(0,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
date.range=as.Date(c("2019-01-01","2019-12-31"));xmaj.date=seq(date.range[1],date.range[2],"2 months");xmin.date=seq(date.range[1],date.range[2],"1 months")
xlim.val=as.numeric(format(date.range,"%j"));xmaj=as.numeric(format(xmaj.date,"%j"));xmin=as.numeric(format(xmin.date,"%j"))
#xlim.val=c(91,305);by.x=60;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
xlab=format(xmaj.date, format="%b")
#tiff(filename=paste0(plot.path,"tiff/20190917_ArcticSeaIce.tiff"),width=6,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/20190917_ArcticSeaIce.png"),width=6,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,2,0.1,0.1),oma=c(2,1.75,0.5,1));
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

plot(median~DOY,POR.stats,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
with(POR.stats,shaded.range(DOY,decile.10,Q25,"dodgerblue1",lty=0))
with(POR.stats,shaded.range(DOY,Q75,decile.90,"dodgerblue1",lty=0))
with(POR.stats,shaded.range(DOY,Q25,Q75,"indianred1",lty=0))
with(subset(dat,Year==2012),lines(DOY,Extent.10e6km2,col="red",lty=2))
with(subset(dat,Year==2019),lines(DOY,Extent.10e6km2,col="dodgerblue1",lwd=2))

axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,line=-0.5,xmaj,xmin,xlab);
#axis_fun(1,line=-0.5,xmaj,xmin,xmaj);
box(lwd=1)
mtext(side=2,line=2,"Extent (1x10\u2077 km\u00B2)")
mtext(side=1,line=2,"Date")

plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend.text=c("Interdecile Range","Interquantile Range","2012 (Record Minimum)","2019 Data")
legend(0.5,0.5,legend=legend.text,pch=c(22,22,NA,NA),col=c(NA,NA,"red","blue"),lwd=1.5,lty=c(NA,NA,2,1),pt.bg=c(adjustcolor("dodgerblue1",0.25),adjustcolor("indianred1",0.25),"red","dodgerblue1"),pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
text(1,0.1,adj=1,font=3,cex=0.5,xpd=NA,"\u00B9 Data from National Snow and Ice Data Center.\n\n \u00B2  Interdecile Range (10th - 90th quantile)\n and Interquantile range (25th to 75th quantile)\n based on 1981 - 2010 period.")
dev.off()

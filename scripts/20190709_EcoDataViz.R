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
dat=dat[order(dat$Year,dat$Day),]
dat$Date=with(dat,date.fun(paste(Year,format(strptime(Day,"%j"),format="%m-%d"),sep="-"),tz="GMT"))
dat$month.yr=with(dat,date.fun(paste(Year,format(Date,"%m"),01,sep="-"),tz="GMT"))
dat$decade=((dat$Year)%/%10)*10


#tiff(filename=paste0(plot.path,"tiff/20190709_ArcticIce.tiff"),width=7,height=5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/20190709_ArcticIce.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2.5,0.1,1.5),oma=c(1,1.75,0.5,1));
layout(matrix(c(1:3,4,4,5),2,3,byrow=T),widths=c(1,1,0.3),heights=c(0.75,1))

xlim.val=date.fun(c("1979-01-01","2019-07-01"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ylim.val=c(0.5,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Thickness_m~Date,dat,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col="grey80")
abline(h=ymin,v=xmin,lty=2,lwd=0.5,col="grey90")
with(dat,lines(Date,Thickness_m,lwd=2,col=adjustcolor("dodgerblue1",0.5)))
period.mean=ddply(subset(dat,Year!=2019),"Year",summarise,mean.val=mean(Thickness_m,na.rm=T),sd.val=sd(Thickness_m,na.rm=T),N.val=N(Thickness_m))
period.mean$Df=period.mean$N.val-1
period.mean$Tp=abs(qt(1-0.95,period.mean$Df))
period.mean$LCI=with(period.mean,mean.val-sd.val*(Tp/sqrt(N.val)))
period.mean$UCI=with(period.mean,mean.val+sd.val*(Tp/sqrt(N.val)))
#with(period.mean,points(date.fun(paste(Year,"01-01",sep="-"),tz="GMT"),mean.val,pch=19,col="grey"))
with(period.mean,lines(date.fun(paste(Year,"01-01",sep="-"),tz="GMT"),mean.val,lty=1,col="red"))
with(period.mean,lines(date.fun(paste(Year,"01-01",sep="-"),tz="GMT"),LCI,lty=2,col="red"))
with(period.mean,lines(date.fun(paste(Year,"01-01",sep="-"),tz="GMT"),UCI,lty=2,col="red"))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%Y"))
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Ice Thickness (m)")
mtext(side=1,line=1.75,"Date")

ylim.val=c(0,35000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Vol_km3~Date,dat,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col="grey80")
abline(h=ymin,v=xmin,lty=2,lwd=0.5,col="grey90")
with(dat,lines(Date,Vol_km3,lwd=2,col=adjustcolor("dodgerblue1",0.5)))
period.mean=ddply(subset(dat,Year!=2019),"Year",summarise,mean.val=mean(Vol_km3,na.rm=T),sd.val=sd(Vol_km3,na.rm=T),N.val=N(Vol_km3))
period.mean$Df=period.mean$N.val-1
period.mean$Tp=abs(qt(1-0.95,period.mean$Df))
period.mean$LCI=with(period.mean,mean.val-sd.val*(Tp/sqrt(N.val)))
period.mean$UCI=with(period.mean,mean.val+sd.val*(Tp/sqrt(N.val)))
#with(period.mean,points(date.fun(paste(Year,"01-01",sep="-"),tz="GMT"),mean.val,pch=19,col="grey"))
with(period.mean,lines(date.fun(paste(Year,"01-01",sep="-"),tz="GMT"),mean.val,lty=1,col="red"))
with(period.mean,lines(date.fun(paste(Year,"01-01",sep="-"),tz="GMT"),LCI,lty=2,col="red"))
with(period.mean,lines(date.fun(paste(Year,"01-01",sep="-"),tz="GMT"),UCI,lty=2,col="red"))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%Y"))
axis_fun(2,ymaj,ymin,ymaj/1e3);box(lwd=1)
mtext(side=2,line=2,"Ice Volume (x10\u00B3 km\u00B3)")
mtext(side=1,line=1.75,"Date")

plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend.text=c("Daily Data","Annual Mean","\u00B195% CI")
legend(0.5,0.5,legend=legend.text,col=c(adjustcolor("dodgerblue1",0.5),"red","red"),lwd=c(2,1,1),lty=c(1,1,2),ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

xlim.val=c(0,35000);by.x=5000;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,3);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
N.yrs=length(unique(dat$Year))
yrs.val=seq(1979,2019,1)
decades=c(1980,1990,2000,2010)
cols=colorRampPalette(c("dodgerblue1","indianred1"))(N.yrs)
plot(Thickness_m~Vol_km3,dat,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n",xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col="grey80")
abline(h=ymin,v=xmin,lty=2,lwd=0.5,col="grey90")
for(i in 1:N.yrs){
  with(subset(dat,Year==yrs.val[i]),points(Vol_km3,Thickness_m,pch=21,bg=adjustcolor(cols[i],0.2),col=adjustcolor(cols[i],0.4),lwd=0.1,cex=1.25))
}
min.vals=ddply(subset(dat,decade%in%decades),"Year",summarise,min.vol=min(Vol_km3,na.rm=T),min.thick=min(Thickness_m,na.rm = T))
for(i in 1:length(decades)){
  with(subset(min.vals,Year==decades[i]),text(min.vol-1000,min.thick+0.5,decades[i],adj=1,font=3))
  with(subset(min.vals,Year==decades[i]),lines(c(min.vol,min.vol-1000),c(min.thick,min.thick+0.5),col="grey",lwd=2))
  with(subset(min.vals,Year==decades[i]),points(min.vol,min.thick,pch=19,col="grey"))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj/1e3,cex=1.1)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=2,"Ice Volume (x10\u00B3 km\u00B3)")
mtext(side=2,line=2,"Ice Thickness (m)")

legend_image=as.raster(matrix(cols,ncol=1))
plot(c(0,1),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
text(x=0.6, y = c(0.5,0.8), labels = c(2019,1979),cex=0.8,xpd=NA,adj=0)
rasterImage(legend_image, 0.25, 0.5, 0.5,0.8)
text(0.25+(0.5-0.25)/2,0.85,"Year")
legend.text=c("Yearly minimum","Daily Data")
legend(0.5,0.3,legend=legend.text,col=c("grey",adjustcolor(cols[41],0.2)),pt.bg=c(NA,adjustcolor(cols[41],0.4)),pch=c(19,21),pt.cex=c(1,1.5),lty=c(NA,NA),lwd=c(NA,1.5),ncol=1,cex=0.9,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

text(1.7,-0.20,adj=1,font=3,cex=0.65,"PIOMAS Data from the \nPolar Science Center (http://psc.apl.uw.edu/research/projects/arctic-sea-ice-volume-anomaly/)",xpd=NA)
dev.off()

#with(ddply(subset(dat,Year%in%decades),"Year",summarise,min.vol=min(Vol_km3,na.rm=T),min.thick=min(Thickness_m,na.rm = T)),points(min.vol,min.thick))


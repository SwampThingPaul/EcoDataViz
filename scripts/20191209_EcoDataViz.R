## 
## Eco #DataViz (12/9/2019)
## Lake Mendota chlorophyll - LTER data
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


library(rjson)

#Paths
#setwd("D:/_GitHub/PseudoTidyTuesday")
wd="D:/_GitHub/EcoDataViz"

#paths=paste0(getwd(),c("/Plots/","/Data/"))
paths=paste0(wd,c("/Plots/","/Data/20191209/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
data.path=paths[2];


# Data --------------------------------------------------------------------

dat=read.csv(paste0(data.path,"ntl38_v4.csv"))
dat$sampledate=date.fun(as.character(dat$sampledate),tz="America/Chicago")
dat$month=as.numeric(format(dat$sampledate,"%m"))

unique(dat$lakeid)
unique(dat$depth_range_m)
dat.ME=subset(dat,lakeid=="ME")
summary(dat.ME)

ylim.val=c(0,70);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,12);xmaj=seq(xlim.val[1],xlim.val[2],2);xmin=seq(xlim.val[1],xlim.val[2],1)

#png(filename=paste0(plot.path,"png/20191209_LakeMendotaChlorophyll.png"),width=6,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,2,0.1,0.1),oma=c(3,1.75,0.75,1));

plot(mono_chl_spec~month,dat.ME,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(dat.ME,points(month,mono_chl_spec,pch=21,bg=adjustcolor("forestgreen",0.5),col="forestgreen",lwd=0.01,cex=1.25))
with(dat.ME,points(month,correct_chl_fluor,pch=21,bg=NA,col=adjustcolor("grey",0.5),lwd=0.01,cex=1.25))
k=with(dat.ME,loess(mono_chl_spec~month))
x.val=seq(xlim.val[1],xlim.val[2],length.out=24)
k.pred=predict(k,newdata=data.frame(month=x.val),se=T)
UCI=k.pred$fit+qt(0.975,k.pred$df)*k.pred$se.fit
LCI=k.pred$fit-qt(0.975,k.pred$df)*k.pred$se.fit
lines(x.val,k.pred$fit,col="forestgreen",lwd=2)
lines(x.val,UCI,col="forestgreen",lwd=1,lty=2)
lines(x.val,LCI,col="forestgreen",lwd=1,lty=2)

k2=with(dat.ME,loess(correct_chl_fluor~month))
k2.pred=predict(k2,newdata=data.frame(month=x.val),se=T)
UCI2=k2.pred$fit+qt(0.975,k2.pred$df)*k2.pred$se.fit
LCI2=k2.pred$fit-qt(0.975,k2.pred$df)*k2.pred$se.fit

lines(x.val,k2.pred$fit,col="grey",lwd=2)
lines(x.val,UCI2,col="grey",lwd=1,lty=2)
lines(x.val,LCI2,col="grey",lwd=1,lty=2)
axis_fun(1,line=-0.5,xmaj,xmin,month.abb[xmaj])
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=2,line=2,"Chlorophyll-a concentration (\u03BCg L\u207B\u00B9)")
mtext(side=3,"Lake Mendota")
text(xlim.val[1]-1.75,ylim.val[1]-22.5,xpd=NA,adj=0,font=3,cex=0.70,"Data from North Temperate Lakes LTER: Chlorophyll - Madison Lakes Area 1995 - current")

legend("topright",legend=c("Chl-a (Spec)","Chl-a (corrected - Fluorometric)","Spec LOWESS \u00B1 95% CI","Fluor LOWESS \u00B1 95% CI"),
       pch=c(21,21,NA,NA),
       col=c("forestgreen",adjustcolor("grey",0.5),"forestgreen","grey"),
       pt.bg=c(adjustcolor("forestgreen",0.5),NA,NA,NA),
       lwd=c(0.01,0.01,2,2),lty=c(NA,NA,1,1),pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.1)
dev.off()

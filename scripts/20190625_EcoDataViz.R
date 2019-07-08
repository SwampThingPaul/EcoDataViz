## 
## Eco #DataViz (6/25/2019)
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
wd="D:/_GitHub/PseudoTidyTuesday"

#paths=paste0(getwd(),c("/Plots/","/Data/"))
paths=paste0(wd,c("/Plots/","/Data/20190625/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
data.path=paths[2];


# Data --------------------------------------------------------------------
# Using Code adpated from https://github.com/datawrapper/snippets/tree/3a38e1d07af7a00f1f8c1acd0f29e45b5742c947/2019-06-greenland-ice-melting
# adapted from https://github.com/lisacharlotterost

years = seq(1979, 2019)
green.melt =data.frame()

for(i in 1:length(years)){
d1=fromJSON(file=paste0('https://nsidc.org/api/greenland/melt_area/', years[i]))
tmp=data.frame(Date=as.Date(names(unlist(d1))), MeltArea.sqkm=unlist(d1))
row.names(tmp)=1:nrow(tmp)
green.melt=rbind(tmp,green.melt)
}
#write.csv(green.melt, paste0(data.path,"greenland_ice.csv"),row.names=F)

#Handle date
green.melt$Date=date.fun(green.melt$Date,tz="GMT")
green.melt$DOY=format(green.melt$Date,"%j")
green.melt$CY=as.numeric(format(green.melt$Date,"%Y"))
green.melt=green.melt[order(green.melt$Date),]
green.melt$CY.cum.melt=with(green.melt,ave(MeltArea.sqkm,CY,FUN=function(x) cumsum(x)))


greenland.ice.area=1710000
green.melt$MeltArea.per=(green.melt$MeltArea.sqkm/greenland.ice.area)*100

green.melt.POR.stats=ddply(subset(green.melt,CY%in%seq(1979,2018,1)),"DOY",summarise,N.val=N(MeltArea.per),
                            decile.10=quantile(MeltArea.per,probs=0.1,na.rm=T),
                            decile.90=quantile(MeltArea.per,probs=0.9,na.rm=T),
                            Q25=quantile(MeltArea.per,probs=0.25,na.rm=T),
                            Q75=quantile(MeltArea.per,probs=0.75,na.rm=T),
                            median=median(MeltArea.per,na.rm=T))

green.melt.ref.stats=ddply(subset(green.melt,CY%in%seq(1981,2010,1)),"DOY",summarise,
                           N.val=N(MeltArea.per),
                           median=median(MeltArea.per,na.rm=T))


# Plot --------------------------------------------------------------------
# Replicate http://nsidc.org/greenland-today/
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
date.range=as.Date(c("2019-04-01","2019-11-01"));xmaj.date=seq(date.range[1],date.range[2],"2 months");xmin.date=seq(date.range[1],date.range[2],"1 months")
xlim.val=as.numeric(format(date.range,"%j"));xmaj=as.numeric(format(xmaj.date,"%j"));xmin=as.numeric(format(xmin.date,"%j"))
#xlim.val=c(91,305);by.x=60;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
xlab=format(xmaj.date, format="%b")
#tiff(filename=paste0(plot.path,"tiff/20190625_greenland.tiff"),width=6,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/20190625_greenland.png"),width=6,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,2,0.1,0.1),oma=c(2,1.75,0.5,1));
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

plot(median~DOY,green.melt.POR.stats,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
with(green.melt,points(DOY,MeltArea.per,pch=21,bg=adjustcolor("grey80",0.1),col=adjustcolor("grey",0.5),lwd=0.5,cex=0.7))
with(green.melt.POR.stats,shaded.range(DOY,decile.10,Q25,"dodgerblue1",lty=0))
with(green.melt.POR.stats,shaded.range(DOY,Q75,decile.90,"dodgerblue1",lty=0))
with(green.melt.POR.stats,shaded.range(DOY,Q25,Q75,"indianred1",lty=0))
with(green.melt.ref.stats,lines(DOY,median,col="blue",lwd=2,lty=2))
with(subset(green.melt,CY==2019),lines(DOY,MeltArea.per,col="red",lwd=2))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,line=-0.5,xmaj,xmin,xlab);
#axis_fun(1,line=-0.5,xmaj,xmin,xmaj);
box(lwd=1)
mtext(side=2,line=2,"Melt Area (% of Greenland Ice Sheet)")
mtext(side=1,line=2,"Day of the Year")

plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend.text=c("Data","Interdecile Range","Interquantile Range","1980 - 2010 Median", "2019 Data")
legend(0.5,0.5,legend=legend.text,pch=c(21,22,22,NA,NA),col=c(adjustcolor("grey",0.5),NA,NA,"blue","red"),lwd=1.5,lty=c(NA,NA,NA,2,1),pt.bg=c(adjustcolor("grey80",0.1),adjustcolor("dodgerblue1",0.25),adjustcolor("indianred1",0.25),"blue","red"),pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
text(1,0.1,adj=1,font=3,cex=0.5,xpd=NA,"\u00B9 Data from National Snow and Ice Data Center.\n\n \u00B2  Based on ice sheet area of 1.71 x 10\u2076 km\u00B2.\n\n \u00B3 Interdecile Range (10th - 90th quantile)\n and Interquantile range (25th to 75th quantile)\n based on 1979 - 2018 period.")
dev.off()


# Annual Mean and CI 
alpha=0.95
greenland.melt.mean=ddply(subset(green.melt,CY%in%seq(1979,2018,1)),"CY",summarise,mean.val=mean(MeltArea.sqkm,na.rm=T),sd.val=sd(MeltArea.sqkm,na.rm=T),N.val=N(MeltArea.sqkm))
#greenland.melt.mean=ddply(subset(green.melt,CY%in%seq(1979,2018,1)),"CY",summarise,mean.val=mean(MeltArea.per,na.rm=T),sd.val=sd(MeltArea.per,na.rm=T),N.val=N(MeltArea.per))
greenland.melt.mean$Df=greenland.melt.mean$N.val-1
greenland.melt.mean$Tp=with(greenland.melt.mean,abs(qt(1-alpha,Df)))
greenland.melt.mean$LCI=with(greenland.melt.mean,mean.val-sd.val*(Tp/sqrt(N.val)))
greenland.melt.mean$UCI=with(greenland.melt.mean,mean.val+sd.val*(Tp/sqrt(N.val)))

with(greenland.melt.mean,cor.test(mean.val,CY,method="kendall"))

ylim.val=c(2.5e4,2.5e5);by.y=5e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1979,2019);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
#tiff(filename=paste0(plot.path,"tiff/20190625_annualmean_greenland.tiff"),width=6,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/20190625_annualmean_greenland.png"),width=6,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,2,0.1,0.1),oma=c(2,1.75,0.5,1));

plot(mean.val~CY,greenland.melt.mean,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
with(greenland.melt.mean,shaded.range(CY,LCI,UCI,"grey",lty=1))
with(greenland.melt.mean,lines(CY,mean.val,col="indianred1",lwd=1.5))
with(greenland.melt.mean,points(CY,mean.val,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,ymaj,ymin,ymaj/1e4)
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);
box(lwd=1)
mtext(side=2,line=2.5,"Mean Greenland Ice Melt Area (x10\u2074 km\u00B2)")
mtext(side=1,line=2.0,"Calendar Year")
legend("topleft",legend=c("Annual Mean","\u00B1 95% CI"),pch=c(21,22),col=c("black","grey"),lwd=0.1,lty=c(NA,NA),pt.bg=c("indianred1",adjustcolor("grey",0.25)),pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
text(xlim.val[2]+3,ylim.val[1]-5.75e4,adj=1,font=3,cex=0.5,xpd=NA,"\u00B9 Data from National Snow and Ice Data Center.")
dev.off()


## Day of Peak melt 
max.melt.CY=data.frame()
for(i in 1:length(years[1:40])){
  CY.max=max(subset(green.melt,CY==years[i])$MeltArea.sqkm,na.rm=T)
  tmp.dat=subset(green.melt,CY==years[i]&MeltArea.sqkm==CY.max)
  
  tmp.dat.final=data.frame(CY=years[i],DOY.max.melt=as.numeric(min(tmp.dat$DOY,na.rm=T)),max.melt=CY.max)
  max.melt.CY=rbind(tmp.dat.final,max.melt.CY)
}
with(max.melt.CY,cor.test(DOY.max.melt,CY,method="kendall"))

ylim.val=c(150,249);by.y=30;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1979,2019);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
#tiff(filename=paste0(plot.path,"tiff/20190625_DOY_peak_greenland.tiff"),width=6,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/20190625_DOY_peak_greenland.png"),width=6,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,4.5,0.1,0.1),oma=c(2,1.75,0.5,1));
plot(DOY.max.melt~CY,max.melt.CY,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
with(max.melt.CY,lines(CY,DOY.max.melt,lty=2,lwd=1.5,col="dodgerblue1"))
with(max.melt.CY,points(CY,DOY.max.melt,pch=21,lwd=0.01,cex=1.25,bg="dodgerblue1"))
axis_fun(1,line=-0.5,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,format(as.Date(ymaj,origin="2018-01-01"),"%B %d"))
box(lwd=1)
mtext(side=1,line=2,"Calendar Year")
mtext(side=2,line=4.25,"Day of Peak Melt")
dev.off()
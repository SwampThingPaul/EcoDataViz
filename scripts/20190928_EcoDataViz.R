## 
## Eco #DataViz (9/28/2019)
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

#GIS Libraries
library(rgdal)
library(rgeos)

#Paths
wd="D:/_GitHub/EcoDataViz"

paths=paste0(wd,c("/Plots/","/Export/","/Data/20190928/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path="D:/_GISData/" 

utm17=CRS("+proj=utm +zone=17 +datum=WGS84 +units=m")
wgs84=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# GIS Data ----------------------------------------------------------------
#  Due to file sizes associated with shapefiles, these data will not be placed 
# on GitHub.

shore=spTransform(readOGR(paste0(GIS.path,"FWC"),"FWC_Shoreline"),utm17)
roads=spTransform(readOGR(paste0(GIS.path,"FDOT"),"FDOT_Roads"),utm17)
canals=spTransform(readOGR(paste0(GIS.path,"SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),utm17)
wmd.monitoring=spTransform(readOGR(paste0(GIS.path,"SFWMD_Monitoring_20190909"),"Environmental_Monitoring_Stations"),utm17)

wx.site=subset(wmd.monitoring,SITE%in%c("S78W","AVEMAR","S75WX")&ACTIVITY_S=="Weather")
garden=SpatialPointsDataFrame(data.frame(Long=-81.6017,Lat=26.5797),data=data.frame(SITE="Home"),proj4string = wgs84)
garden=spTransform(garden,utm17)

bbox.lims=bbox(gBuffer(wx.site,width=500))
plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(canals,col="lightblue",lwd=3,add=T)
plot(canals,col="blue",lwd=0.75,add=T)
plot(roads,col="grey50",lty=2,add=T)
plot(wx.site,pch=21,bg="indianred1",cex=2,lwd=0.1,add=T)
text(wx.site,"STATION",pos=4,font=2,halo=T)

plot(garden,pch=23,bg="yellow",cex=2,add=T)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4)

# Data --------------------------------------------------------------------
dates=as.Date(c("2008-05-01","2019-04-30"))
dates2=seq(dates[1],dates[2],"1 months")

dbkeys=data.frame(SITE=c("S78WX","FPWX","S75WX","AVEMARIA"),DBKEY=c("IY924","IX319","OQ984","VW745"))
#dbkeys=subset(dbkeys,SITE!="FPWX")

# The following code with nest loops downloads data directly from SFWMD...its takes a while.
# to avoid such a long wait, provided in the data folder is the associated data. 

#temp.dat=data.frame()
#for(i in 1:nrow(dbkeys)){
#  for(j in 2:length(dates2)){
#    tmp=DBHYDRO_breakpoint(dates2[j-1],dates2[j],dbkeys$DBKEY[i])
#    temp.dat=rbind(tmp,temp.dat)
#    print(j)
#  }
#  print(i)
#}
#write.csv(temp.dat,paste0(export.path,"airtemp.csv"),row.names = F)

temp.dat=read.csv(paste0(data.path,"airtemp.csv"))
temp.dat$DATE=date.fun(as.character(temp.dat$DATE))
temp.dat$DATETIME=date.fun(as.character(temp.dat$DATETIME),form="%F %X")

temp.dat$WY=WY(temp.dat$DATE)
temp.dat$hour=format(temp.dat$DATETIME,"%H")
temp.dat$hour.date=with(temp.dat,date.fun(paste0(DATE," ",hour,":00:00"),form="%F %H:%M:%S"))

hour.dat=ddply(merge(temp.dat,dbkeys,"DBKEY"),c("SITE","WY","hour.date"),summarise,min.Temp=min(Data.Value,na.rm=T),mean.Temp=mean(Data.Value,na.rm=T))
hour.dat$chill.hours=with(hour.dat,ifelse(min.Temp<7,1,0))

annual.chill=ddply(hour.dat,c("SITE","WY"),summarise,TChill=sum(chill.hours,na.rm=T))
SITES=c("S75WX","S78WX","AVEMARIA")
annual.chill2=ddply(subset(annual.chill,SITE%in%SITES),c("WY"),summarise,mean.TChill=mean(TChill,na.rm=T))
plot(mean.TChill~WY,annual.chill2,type="l",las=2)

m=mean(annual.chill2$mean.TChill)
s=sd(annual.chill2$mean.TChill)
N.val=N(annual.chill2$mean.TChill)
Df=N.val-1
Tp.95=abs(qt(1-0.95,Df));Tp.95
up.lim.95=m+s*(Tp.95/sqrt(N.val));up.lim.95
low.lim.95=m-s*(Tp.95/sqrt(N.val));low.lim.95

#cols=colorRampPalette(c("dodgerblue1","indianred1"))(length(SITES))
cols=c("dodgerblue1","darkolivegreen3","indianred1")
ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2009,2019);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
#png(filename=paste0(plot.path,"chill_hours.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
#tiff(filename=paste0(plot.path,"chill_hours.tiff"),width=6.5,height=4.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(2.5,2,0.25,0.5),oma=c(2,2,1.5,0.75),xpd=F);
layout(matrix(c(1,1,2,3),2,2),widths=c(1,0.5))

plot(mean.TChill~WY,annual.chill2,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
shaded.range(c(2007.5,2019.5),rep(low.lim.95,2),rep(up.lim.95,2),"darkolivegreen2",lty=1)
lines(c(2007.5,2019.5),rep(m,2),lty=2)
#shaded.range(c(2008,2020),rep(200,2),rep(300,2),"forest green",lty=1)
for(i in 1:length(SITES)){
  with(subset(annual.chill,SITE==SITES[i]),pt_line(WY,TChill,2,cols[i],2,21,cols[i],cex=1.25))
}
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2.75, "Water Year\n(May - Apirl)")
mtext(side=2,line=2.75,"Chill Hours (Hours Year\u207B\u00B9)")

bbox.lims=bbox(gBuffer(wx.site,width=1000))
plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(canals,col="lightblue",lwd=1.5,add=T)
plot(canals,col="blue",lwd=0.25,add=T)
plot(roads,col="grey50",lty=2,lwd=0.5,add=T)
plot(wx.site,pch=21,bg="indianred1",cex=1.5,lwd=0.1,add=T)
text(wx.site,"STATION",pos=4,halo=T,cex=0.75)
plot(garden,pch=4,cex=1,add=T)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4)

plot(0:1,0:1,axes=F,ylab=NA,xlab=NA,type="n")
leg.text=c("S75WX","S78WX","AVEMARIA","95% CI")
legend(0.5,0.5,legend=leg.text,
       pch=c(NA),
       col=c(cols,"black"),
       pt.bg=NA,
       lwd=c(rep(1.25,3),1),lty=c(2),
       pt.cex=1.5,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="white")
legend(0.5,0.5,legend=leg.text,
       pch=c(21,21,21,22),
       col=c("black","black","black","darkolivegreen2"),
       pt.bg=c(cols,adjustcolor("darkolivegreen2",0.25)),
       lwd=c(0.1,0.1,0.1,0.1),lty=c(NA),
       pt.cex=1.5,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
text(1,0,adj=1,font=3,cex=0.8,xpd=NA,"Data Source: SFWMD")
dev.off()
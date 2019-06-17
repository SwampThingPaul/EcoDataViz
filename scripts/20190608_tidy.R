## 
## "Tidy Tuesday (06/18/2019)
##
## Code was compiled by Paul Julian
## contact info: pjulian@ufl.edu

#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")
library(plyr)
library(reshape)

#Paths
setwd("D:/UF/TidyTuesday")

paths=paste0(getwd(),c("/Plots/","/Data/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
data.path=paths[2]


# Data --------------------------------------------------------------------
dat=read.csv(paste0(data.path,"20190618/queryResults.csv"))
dat$sampleDate=date.fun(dat$sampleDate,form="%m/%d/%Y %H:%M:%S",tz="America/Chicago")
dat$DOY=format(dat$sampleDate,"%j")
dat$CY=as.numeric(format(dat$sampleDate,"%Y"))

subset(dat,is.na(result)==T)
subset(dat,is.na(detectLimit)==T)
dat$HalfMDL=with(dat,ifelse(result<detectLimit,detectLimit/2,result))

sites.dat=ddply(dat,c("siteID","name"),summarise,N.val=N(fraction),min.date=date.fun(min(sampleDate),tz="America/Chicago"),max.date=date.fun(max(sampleDate),tz="America/Chicago"),max.microcystin=max(HalfMDL,na.rm=T))
sites.dat$yrs.dat=with(sites.dat,as.numeric(max.date-min.date))*3.17098e-8
#longterm.sites=subset(sites.dat,yrs.dat>=13)
longterm.sites=subset(sites.dat,yrs.dat>=13&max.microcystin>300)

dat=dat[order(dat$sampleDate),]
dat=subset(dat,siteID%in%longterm.sites$siteID)

yrs=seq(2006,2018,1)
site.labs=paste(unlist(stringr::str_split(longterm.sites$name," Beach "))[seq(1,14,2)],"Beach")
ylim.val=c(0.05,2000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(120,245);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
#tiff(filename=paste0(plot.path,"20190617_iowaDNR_mycrosystin.tiff"),width=7,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1.5,2.25,0.5,0.1),oma=c(2,1.75,0.7,1));
layout(matrix(1:8,2,4,byrow=T))

pts.bg=adjustcolor("grey",0.25)
pts.col=adjustcolor("black",0.25)
for(i in 1:7){
  plot(HalfMDL~DOY,dat,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  tmp.dat=subset(dat,CY%in%yrs&siteID==longterm.sites$siteID[i])
  with(tmp.dat,points(DOY,HalfMDL,pch=21,bg=pts.bg,col=pts.col,lwd=0.1,cex=1.25))
  k=with(tmp.dat,loess(log(HalfMDL)~DOY))
  x.val=seq(xlim.val[1],xlim.val[2],7)
  k.pred=predict(k,newdata=data.frame(DOY=x.val),se=T)
  UCI=exp(k.pred$fit+qt(0.975,k.pred$df)*k.pred$se.fit)
  LCI=exp(k.pred$fit-qt(0.975,k.pred$df)*k.pred$se.fit)
  shaded.range(x.val,LCI,UCI,"dodgerblue1",lty=2,lwd=0.75)
  lines(x.val,exp(k.pred$fit),lwd=1.75,col="dodgerblue")
  #lines(x.val,LCI,lty=2,lwd=1.75,col="dodgerblue")
  #lines(x.val,UCI,lty=2,lwd=1.75,col="dodgerblue")
  abline(h=20,lty=2,col="red")
  with(subset(dat,CY==2019&siteID==longterm.sites$siteID[i]),lines(DOY,HalfMDL,lty=2,col="indianred1",lwd=1.75))
  with(subset(dat,CY==2019&siteID==longterm.sites$siteID[i]),points(DOY,HalfMDL,pch=21,bg="indianred1",cex=1.25,lwd=0.1))
  if(i%in%c(1:3)){axis_fun(1,line=-0.5,xmaj,xmin,NA)}else{axis_fun(1,line=-0.5,xmaj,xmin,xmaj)}
  if(i%in%c(1,5)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)};box(lwd=1)
  mtext(side=3,site.labs[i],cex=0.6)
}
plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend.text=c("2006 - 2018 Grab Samples","2006 - 2018 LOESS (\u00B1 95% CI)","2019 Samples","Iowa State advisory threshold")
leg.pt.bg=c(pts.bg,adjustcolor("dodgerblue1",0.25),"indianred1",NA)
leg.cols1=c("black","dodgerblue1","indianred1","red")
leg.cols2=c("black","dodgerblue1","black","red")
legend(0.5,0.5,legend=legend.text,pch=NA,col=leg.cols1,lwd=1.5,lty=c(NA,1,2,2),pt.bg=leg.pt.bg,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="white")
legend(0.5,0.5,legend=legend.text,pch=c(21,22,21,NA),col=leg.cols2,lwd=0.2,lty=NA,pt.bg=leg.pt.bg,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
text(1,0.1,adj=1,font=3,cex=0.5,"Data from Iowa DNR\n(https://programs.iowadnr.gov/aquia/search)\n most recent data up to June 17, 2019")

mtext(side=2,line=0.5,outer=T,"Microcystin (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=0.5,outer=T,"Day of the Year")
dev.off()


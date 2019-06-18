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
library(RColorBrewer)
library(classInt)

#Paths
setwd("D:/_GitHub/PseudoTidyTuesday")

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
longterm.sites=longterm.sites[order(-longterm.sites$max.microcystin),]

dat=dat[order(dat$sampleDate),]
dat=subset(dat,siteID%in%longterm.sites$siteID)


# Plot --------------------------------------------------------------------
yrs=seq(2006,2018,1)
site.labs=paste(unlist(stringr::str_split(longterm.sites$name," Beach "))[seq(1,14,2)],"Beach")
ylim.val=c(0.05,2000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(120,245);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
#tiff(filename=paste0(plot.path,"tiff/20190617_iowaDNR_mycrosystin.tiff"),width=7,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/20190617_iowaDNR_mycrosystin.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
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
  abline(h=c(8,20),lty=2,col="red")
  with(subset(dat,CY==2019&siteID==longterm.sites$siteID[i]),lines(DOY,HalfMDL,lty=2,col="indianred1",lwd=1.75))
  with(subset(dat,CY==2019&siteID==longterm.sites$siteID[i]),points(DOY,HalfMDL,pch=21,bg="indianred1",cex=1.25,lwd=0.1))
  if(i%in%c(1:3)){axis_fun(1,line=-0.5,xmaj,xmin,NA)}else{axis_fun(1,line=-0.5,xmaj,xmin,xmaj)}
  if(i%in%c(1,5)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)};box(lwd=1)
  mtext(side=3,site.labs[i],cex=0.6)
}
plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend.text=c("2006 - 2018 Grab Samples","2006 - 2018 LOESS (\u00B1 95% CI)","2019 Samples","Iowa State advisory thresholds")
leg.pt.bg=c(pts.bg,adjustcolor("dodgerblue1",0.25),"indianred1",NA)
leg.cols1=c("black","dodgerblue1","indianred1","red")
leg.cols2=c("black","dodgerblue1","black","red")
legend(0.5,0.5,legend=legend.text,pch=NA,col=leg.cols1,lwd=1.5,lty=c(NA,1,2,2),pt.bg=leg.pt.bg,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="white")
legend(0.5,0.5,legend=legend.text,pch=c(21,22,21,NA),col=leg.cols2,lwd=0.2,lty=NA,pt.bg=leg.pt.bg,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
text(1,0.1,adj=1,font=3,cex=0.5,"Data from Iowa DNR\n(https://programs.iowadnr.gov/aquia/search)\n most recent data up to June 17, 2019")

mtext(side=2,line=0.5,outer=T,"Total Microcystin (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=0.5,outer=T,"Day of the Year")
dev.off()



# additional analyses -----------------------------------------------------
seasonal.peak=data.frame()
for(i in 1:length(longterm.sites$siteID)){
  tmp.dat=subset(dat,siteID==longterm.sites$siteID[i])
  for(j in 1:length(yrs)){
    max.val=if(nrow(subset(tmp.dat,CY==yrs[j]))==0){
      tmp.rslt=data.frame(siteID=longterm.sites$siteID[i],CY=yrs[j],max.val=NA,DOY=NA)
      seasonal.peak=rbind(seasonal.peak,tmp.rslt)
    }else{
    max.val=max(subset(tmp.dat,CY==yrs[j])$HalfMDL)
    DOY.val=subset(tmp.dat,CY==yrs[j]&HalfMDL==max.val)$DOY
    tmp.rslt=data.frame(siteID=longterm.sites$siteID[i],CY=yrs[j],max.val=max.val,DOY=as.numeric(DOY.val))
    seasonal.peak=rbind(seasonal.peak,tmp.rslt)
    }
  }
}
ddply(seasonal.peak,"siteID",summarise,kendall=cor.test(CY,DOY,method="kendall")$estimate,pval=cor.test(CY,DOY,method="kendall")$p.val)

## Plot
n=6
pal <- rev(brewer.pal(n, "BuGn"))
pt.cex.vals=rev(seq(1,2.5,l=n))
int=classIntervals(seasonal.peak$max.val,n,style="fixed",fixedBreaks=c(0.2,5,10,20,100,500,1500))
site.labs=paste(unlist(stringr::str_split(longterm.sites$name," Beach "))[seq(1,14,2)],"Beach")

ylim.val=c(120,300);by.y=30;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2005,2019);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
#tiff(filename=paste0(plot.path,"tiff/20190617_iowaDNR_Peakmycrosystin.tiff"),width=7,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"png/20190617_iowaDNR_Peakmycrosystin.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,2.25,0.5,0.1),oma=c(2,1.75,0.7,1));
layout(matrix(1:8,2,4,byrow=T))

for(i in 1:7){
  tmp.dat=subset(seasonal.peak,siteID==longterm.sites$siteID[i])
  plot(DOY~CY,tmp.dat,axes=F,ylab=NA,xlab=NA,ylim=ylim.val,xlim=xlim.val,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  col=pal[findInterval(tmp.dat$max.val,int$brks,all.inside = T)]
  pt.cex.grad=pt.cex.vals[findInterval(tmp.dat$max.val,int$brks,all.inside = T)]
  with(tmp.dat,lines(CY,DOY,lwd=1.75,col="grey"))
  with(tmp.dat,points(CY,DOY,pch=21,bg=col,cex=pt.cex.grad,lwd=0.1))
  if(i%in%c(1:3)){axis_fun(1,line=-0.5,xmaj,xmin,NA)}else{axis_fun(1,line=-0.5,xmaj,xmin,xmaj)}
  if(i%in%c(1,5)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)};box(lwd=1)
  mtext(side=3,site.labs[i],cex=0.6)
}

plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
labs=NA
mc.conc=c(0.2,5,10,20,100,500,1500)
for(i in 1:n){labs[i]=paste(mc.conc[i],mc.conc[i+1],sep=" - ")}
labs[1]="< 5"
legend(0.5,0.5,legend=rev(labs),pch=c(21),pt.bg=pal,lwd=0.1,lty=NA,pt.cex=pt.cex.vals,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title="Peak Total Microcystin\nConcentration(\u03BCg L\u207B\u00B9)")
text(1,0.1,adj=1,font=3,cex=0.5,"Data from Iowa DNR\n(https://programs.iowadnr.gov/aquia/search)\n most recent data up to June 17, 2019")
mtext(side=2,line=0.5,outer=T,"Day of Year Peak Microcystin Conc.")
mtext(side=1,line=0.5,outer=T,"Calendar Year")
dev.off()
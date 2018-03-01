## Private Class Methods for FLIC Class V2
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)


BinFeedingData.Well.Licks<-function(dfm,well,binsize.min,range=c(0,0)){
  tmp<-FeedingData.Licks(dfm,range)
  cname=paste("W",well,sep="")
  
  tmp<-tmp[,c("Minutes",cname)]
  
  m.min<-min(tmp$Minutes)
  m.max<-max(tmp$Minutes)
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  z<-cut(tmp$Minutes,y,include.lowest=TRUE)
  
  r.min<-aggregate(tmp$Minutes~z,FUN=mean)
  r.A<-aggregate(tmp[,cname]~z,FUN=sum)
  
  results<-data.frame(r.min,r.A[,2])
  names(results)<-c("Interval","Min","SumLicks")
  results
}

BinFeedingData.Licks<-function(dfm,binsize.min,range=c(0,0)){
  result<-BinFeedingData.Well.Licks(dfm,1,binsize.min,range)

  for(i in 2:12) {
    cname=paste("W",i,sep="")
    tmp<-BinFeedingData.Well.Licks(dfm,i,binsize.min,range)
    result<-data.frame(result,tmp$SumLicks)
  }
  names(result)<-c("Interval","Min",paste("W",1:12,sep=""))
  result  
}

BinFeedingData.Well.Events<-function(dfm,well,binsize.min,range=c(0,0)){
  tmp<-FeedingData.Events(dfm,range)
  cname=paste("W",well,sep="")
  
  tmp<-tmp[,c("Minutes",cname)]
  ## Remember that Event data include duration, but we aren't interested
  ## in that.  Set values >0 to 1.
  tmp[tmp[,cname]>1,cname]<-1
  
  m.min<-min(tmp$Minutes)
  m.max<-max(tmp$Minutes)
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  z<-cut(tmp$Minutes,y,include.lowest=TRUE)
  
  r.min<-aggregate(tmp$Minutes~z,FUN=mean)
  r.A<-aggregate(tmp[,cname]~z,FUN=sum)
  
  results<-data.frame(r.min,r.A[,2])
  names(results)<-c("Interval","Min","SumEvents")
  results
}

BinFeedingData.Events<-function(dfm,binsize.min,range=c(0,0)){
  result<-BinFeedingData.Well.Events(dfm,1,binsize.min,range)
  
  for(i in 2:12) {
    cname=paste("W",i,sep="")
    tmp<-BinFeedingData.Well.Licks(dfm,i,binsize.min,range)
    result<-data.frame(result,tmp$SumLicks)
  }
  names(result)<-c("Interval","Min",paste("W",1:12,sep=""))
  result  
}
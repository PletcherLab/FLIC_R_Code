## Private Class Methods for FLIC Class V2
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)


#####Treatment based functions######

#####Binning functions######
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
  
  #tmp<-aggregate(tmp$ElapsedHours,by=list(Channel=tmp$Channel),max)
  
  
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

OutputBinnedFeeding.Monitors<-function(monitors,parameters,binsize.min,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  for(j in 1:length(monitors)){
    ##print(paste("Outputting Interval Data for DFM ",j,".",sep=""))
    ##flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    pnames<-Get.Parameter.Names(p)
    parameter.vector<-GetParameterVector(p)
    tmp<-BinFeedingData.Licks(dfm,binsize.min,range)
    DFM<-rep(monitor,nrow(tmp))
    tmp<-data.frame(DFM,tmp)
    
    tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
    tmp3<-cbind(tmp,tmp2)
    names(tmp3)<-c(names(tmp),pnames)
    if(j==1){
      result<-tmp3
    }
    else {
      result<-rbind(result,tmp3)
    }
  }
  
  filename<-paste("BinnedFeeding_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}
OutputBinnedEvents.Monitors<-function(monitors,parameters,binsize.min,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  for(j in 1:length(monitors)){
    ##print(paste("Outputting Interval Data for DFM ",j,".",sep=""))
    ##flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    pnames<-Get.Parameter.Names(p)
    parameter.vector<-GetParameterVector(p)
    tmp<-BinFeedingData.Events(dfm,binsize.min,range)
    
    DFM<-rep(monitor,nrow(tmp))
    tmp<-data.frame(DFM,tmp)
    tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
    tmp3<-cbind(tmp,tmp2)
    names(tmp3)<-c(names(tmp),pnames)
    
    if(j==1){
      result<-tmp3
    }
    else {
      result<-rbind(result,tmp3)
    }
  }
  
  filename<-paste("BinnedEvents_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}










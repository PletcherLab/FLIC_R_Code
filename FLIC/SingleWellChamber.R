## Private Class Methods for FLIC Class V2
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)


#####Treatment based functions######

#####Binning functions######
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
GetCumLicksPlots.DFM<-function(dfm,FacetPlots=TRUE,TransformLicks=TRUE){
  tmp<-Feeding.Durations.Well(dfm,1)
  SumLicks<-cumsum(tmp$Licks)
  if(TransformLicks==TRUE)
    SumLicks<-SumLicks^0.25
  row<-1
  col<-1
  Row<-rep(row,nrow(tmp))
  Col<-rep(col,nrow(tmp))
  Well<-rep(1,nrow(tmp))
  tmp<-data.frame(tmp,SumLicks,Row,Col,Well)
  for(i in 2:12){
    if(i%%2==1) row<-row+1
    col<-col+1
    if(col>2) col<-1
    tmp2<-Feeding.Durations.Well(dfm,i)
    if(is.data.frame(tmp2)) {
      x<-tmp2$Minutes
      y<-cumsum(tmp2$Licks)
      SumLicks<-y
      if(TransformLicks==TRUE)
        SumLicks<-SumLicks^0.25
      Row<-rep(row,length(x))
      Col<-rep(col,length(x))
      Well<-rep(i,length(x))
      tmp2<-data.frame(tmp2,SumLicks,Row,Col,Well)
      tmp<-rbind(tmp,tmp2) 
    }
    else {
      fake.entry1<-c(FirstSampleData(dfm)$Minutes,0,0,0,0,0,0,0,0,row,col,i)
      fake.entry2<-c(LastSampleData(dfm)$Minutes,0,0,0,0,0,0,0,0,row,col,i)
      fake.entry<-data.frame(rbind(fake.entry1,fake.entry2))
      names(fake.entry)<-names(tmp)
      tmp<-rbind(tmp,fake.entry) 
    }
  }
  if(TransformLicks==TRUE)
    ylabel="Transformed Cumulative Licks"
  else
    ylabel="Cumulative Licks"
  if(FacetPlots==TRUE) {
  gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line() + facet_grid(rows=vars(Row),cols=vars(Col)) +geom_point() +
    ggtitle(paste("DFM",dfm$ID)) + ylab(ylabel)
  }
  else {
  gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line(size=1.2) +
    ggtitle(paste("DFM",dfm$ID))+ ylab(ylabel)
  }
  gp
}
GetTotalLicksPlot.Monitors<-function(monitors, p, range=c(0,0),TransformLicks=TRUE){
  tmp2<-Feeding.Summary.Monitors(monitors,p,range=range)
  tmp2<-tmp2$Results
  ylabel="Licks"
  if(TransformLicks==TRUE) {
    tmp2$Licks<-tmp2$Licks^0.25
    ylabel="Transformed Licks"
  }
  tmp2$DFM<-factor(tmp2$DFM)
  gp<-ggplot(tmp2,aes(x=DFM,y=Licks,fill=DFM)) + geom_dotplot(binaxis='y',stackdir='center',stackratio=1.5, dotsize=0.7) +
    ylab(ylabel)
  gp
}
PlotBins.Licks<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE){
  binnedData<-BinFeedingData.Licks(dfm,binsize.min,range)
  tmp<-melt(binnedData,id.vars=c("Interval","Min"))
  ylabel<-"Licks"
  xlabel<-"Minutes"
  ttl<-paste("DFM:",dfm$ID)
  if(TransformLicks==TRUE) {
    tmp$value<-tmp$value^0.25
    ylabel<-"Transformed Licks"
    ttl<-paste("DFM:",dfm$ID," (Transformed)")
  }
    
  gp<-ggplot(tmp,aes(x=Min,y=value,fill=variable)) + geom_bar(stat="identity") + facet_grid(variable ~ .) + ggtitle(paste("DFM:",dfm$ID)) +
    theme(legend.position = "none") + ylab(ylabel) + xlab(xlabel)
  show(gp)
}


#####Private functions######
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











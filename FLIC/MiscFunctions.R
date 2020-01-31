require(ggplot2)
require(stats)
require(gridExtra)

## Give filename to this function and it will do analyses of 
## variance with and without DFM as a factor.  It will also plot
## the data for each well for each treatment in each DFM to look for
## consistency.
QuickAOV.FeedingSummary<-function(datafile="FeedingSummary.csv"){
  data<-read.csv(datafile)
  data$Licks<-data$Licks^0.25
  data$DFM<-factor(data$DFM)
  
  aov.1<-aov(Licks~Treatment,data=data)
  print(summary(aov.1))
  cat("\n********\n")
  aov.2<-aov(Licks~DFM+Treatment,data=data)
  print(summary(aov.2))
  
  g1<-ggplot(data,aes(x=DFM,y=Licks)) + geom_boxplot() + geom_jitter(aes(color=Treatment))
  show(g1)
  
  
  g2<-ggplot(data,aes(x=DFM,y=Licks, fill=Treatment)) + geom_boxplot(aes(fill=Treatment))
  show(g2)
}
PlotLicksandLight.Well<-function(dfm,well,range=c(0,0),TransformLicks=TRUE){
  tmp<-FeedingData.Well.Licks(dfm,well)
  SumLicks<-cumsum(tmp)
  if(TransformLicks==TRUE)
    SumLicks<-SumLicks^0.25
  Lights<-GetLightsInfo(dfm)
  Light<-Lights[,paste("W",well,sep="")]
  row<-1
  col<-1
  Row<-rep(row,length(tmp))
  Col<-rep(col,length(tmp))
  Well<-rep(1,length(tmp))
  Size <- Light*2
  Size[Size==0]<-1
  results<-data.frame(dfm$LickData$Minutes,SumLicks,Light,Size,Row,Col,Well)
  names(results)<-c("Minutes","SumLicks","Light","Size","Row","Col","Well")
  if(sum(range)!=0) {
    results<- results[(results$Minutes>range[1]) & (results$Minutes<range[2]),]
  }   
  if(TransformLicks==TRUE)
    ylabel="Transformed Cumulative Licks"
  else
    ylabel="Cumulative Licks"
  gp<-ggplot(results,aes(Minutes,SumLicks,color=Light)) + geom_point(size=results$Size) +
    ggtitle(paste("DFM ",dfm$ID, "; Well ",well, sep=""))+ ylab(ylabel)+ labs(color="Light")
  
  gp
}


GetLightsInfo<-function(dfm){
  Column1<-dfm$RawData$OptoCol1
  Column2<-dfm$RawData$OptoCol2
  data<-data.frame(Column1,Column2)
  data2<-apply(data,1,LightStatus)
  data2<-t(data2)
  final.data<-data.frame(dfm$RawData$Minutes,data2,data)
  names(final.data)<-c("Minutes",paste("W",1:12,sep=""),"OptoCol1","OptoCol2")
  final.data
}
LightStatus<-function(cols){
  col1<-cols[1]
  col2<-cols[2]
  lights<-rep(FALSE,12)
  for(i in 0:5){
    tmp<-bitwShiftL(1,i)
    if(bitwAnd(tmp,col1)>0){
      lights[i*2+1]<-TRUE
    }
    if(bitwAnd(tmp,col2)>0){
      lights[i*2+2]<-TRUE
    }
  }
  lights
}


ScrollEventPlots<-function(dfm,wellnum){
  events<-unique(dfm$BaselineData$Sample)
  currentevent<-1
  keepgoing<-TRUE
  while (keepgoing==TRUE){
    cat("Return = Forward; b = back; q = quit")
    PlotSingleSampleEvent(dfm,wellnum,events[currentevent])
    tmp<-readline()
    if(tmp[1]==""){
      currentevent <- currentevent+1
      if(currentevent>length(events))
        currentevent<-1
    }
    else if(tmp[1]=="b"){
      currentevent<-currentevent-1
      if(currentevent<1)
        currentevent=length(events)
    }
    
    else if(tmp[1]=="q"){
      keepgoing<-FALSE
    }
  }
}


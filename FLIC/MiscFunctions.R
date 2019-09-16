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


GetLightsInfo<-function(dfm){
  Column1<-dfm$RawData$OptoCol1
  Column2<-dfm$RawData$OptoCol2
  data<-data.frame(Column1,Column2)dim(da)
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
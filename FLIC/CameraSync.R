OutputEventPlots<-function(dfm,wellnum){
  w<-paste("W",wellnum,sep="")
  maxRows<-0
  ylims<-c(0,max(dfm$BaselineData[,(6+wellnum)]))
  xlims<-c(0,max(table(dfm$BaselineData$Sample)))
  if(!dir.exists("Plots"))
    dir.create("Plots")
  tmp.dir<-paste(paste("Plots/Well_",wellnum,sep=""))
  if(dir.exists(tmp.dir))
    unlink(tmp.dir,recursive=TRUE)
  dir.create(tmp.dir)
  fn<-paste("Plots/Well_",wellnum,"/",sep="")
  for(i in 1:max(dfm$RawData$Sample)){
    tmp<-subset(dfm$BaselineData,dfm$BaselineData$Sample==i)
    data<-tmp[,(6+wellnum)]
    if(max(data)>dfm$Parameters$Feeding.Threshold) {
      print(i)
      fn.long<-paste(fn,"Event_",i,".png",sep="")
      png(fn.long,width=960,height=480)
      tmp$Index<-(tmp$Index-tmp$Index[1])+1   
      gp<-ggplot(tmp,aes(Index,tmp[,(6+wellnum)])) + geom_line(color="red",size=1.2) + facet_wrap(tmp$Sample) +geom_point(color="blue",size=4) +
        ggtitle(paste("DFM: ",dfm$ID,"   Well: ",w,"   Event:",eventnum)) + ylab("Signal") + labs(color="Chamber") +xlim(xlims) + ylim(ylims)
      show(gp)
      graphics.off()
    }
  }
}



ScrollEventPlots<-function(dfm,wellnum){
  events<-unique(dfm$BaselineData$Sample)
  currentevent<-1
  keepgoing<-TRUE
  while (keepgoing==TRUE){
    cat("Return = Forward; b = back; q = quit")
    PlotSingleSampleEvent(dfm,wellnum,events[currentevent],c(0,nrow()))
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



PlotSingleSampleEvent<-function(dfm,wellnum,eventnum,xlims,ylims){
  tmp<-subset(dfm$BaselineData,dfm$BaselineData$Sample==eventnum)
  tmp$Index<-(tmp$Index-tmp$Index[1])+1
  w<-paste("W",wellnum,sep="")
  gp<-ggplot(tmp,aes(Minutes,tmp[,(6+wellnum)])) + geom_line(color="red",size=1.2) + facet_wrap(tmp$Sample) +geom_point(color="blue",size=4) +
    ggtitle(paste("DFM: ",dfm$ID,"   Well: ",w,"   Event:",eventnum)) + ylab("Signal") + labs(color="Chamber") +xlim(xlims) + ylim(ylims)
  show(gp)
}

Set.Durations.And.Intervals.Well<-function(dfm,well){
  data<-BaselineData.Well(dfm,well)
  sample<-SampleData(dfm)
  events<-FeedingData.Well.Events(dfm,well)
  ## Now we need to update the event durations
  ## Indices will be used for summary duration characteristics
  indices<-1:length(events)
  
  indices<-indices[events>0]
  boutDurs<-events[events>0]  
  
  Durations<-0
  
  if(length(boutDurs)>0) {
    max.inten<-rep(0,length(indices))
    min.inten<-rep(0,length(indices))
    sum.inten<-rep(0,length(indices))
    avg.inten<-rep(0,length(indices))    
    var.inten<-rep(0,length(indices))   
    avg.sample<-rep(0,length(indices))   
    for(i in 1:length(indices)){
      dataindex<-indices[i]
      eventlength<-boutDurs[i]
      tmp2<-data[dataindex:(dataindex+(eventlength-1))]
      sample.tmp<-sample[dataindex:(dataindex+(eventlength-1))]
      max.inten[i]<-max(tmp2)
      min.inten[i]<-min(tmp2)
      sum.inten[i]<-sum(tmp2)
      avg.inten[i]<-mean(tmp2)  
      var.inten[i]<-var(tmp2)
      avg.sample[i]<-mean(sample.tmp)
    }
    
    BoutData<-data.frame(min.inten,max.inten,sum.inten,avg.inten,var.inten,avg.sample)
    names(BoutData)<-c("MinIntensity","MaxIntensity","SumIntensity","MeanIntensity","VarIntensity","Sample")
    
    tmp<-BaselineData(dfm)
    tmp<-tmp[indices,]
    Minutes<-tmp$Minutes
    Events<-boutDurs
    Duration<-Events/dfm$Parameters$Samples.Per.Sec
    AvgInten<-BoutData$MeanIntensity
    MaxInten<-BoutData$MaxIntensity
    MinInten<-BoutData$MinIntensity
    SumInten<-BoutData$SumIntensity
    VarInten<-BoutData$VarIntensity
    Sample<-BoutData$Sample
    Durations<-data.frame(Minutes,Events,Duration,SumInten,AvgInten,MinInten,MaxInten,VarInten,Sample)
    names(Durations)<-c("Minutes","Licks","Duration","TotalIntensity","AvgIntensity","MinIntensity","MaxIntensity","VarIntensity","Sample")    
  }
  
  result<-list(Durations=Durations)
  
  ## Now intervals
  
  ## Collapse feeding data to time BETWEEN events.
  boutInt<-Get.Intervals(FeedingData.Well.Licks(dfm,well))  
  
  indices<-1:length(boutInt)
  indices<-indices[boutInt>0]
  boutInt<-boutInt[boutInt>0]
  
  
  spm<-dfm$Parameters$Samples.Per.Sec
  intA<-boutInt/spm
  
  Ints<-0
  
  if(length(intA)>0) {
    tmp<-BaselineData(dfm)
    tmp<-tmp[indices,]
    Minutes<-tmp$Minutes
    Sample<-tmp$Sample
    IntervalSec<-intA
    Ints<-data.frame(Minutes,Sample,IntervalSec)
  }
  
  result<-list(Durations=Durations,Intervals=Ints)
  result
}

SampleData<-function(dfm,range=c(0,0)) {  
  tmp<-dfm$BaselineData$Sample
  if(sum(range)!=0) {
    tmp<- tmp[(dfm$BaselineData$Minutes>range[1]) & (dfm$BaselineData$Minutes<range[2])]
  }    
  tmp  
}

DFMClass<-function(id,parameters,range=c(0,0)) {
  if (!is.numeric(id) || !all(is.finite(id)))
    stop("invalid arguments")
  
  ## Check to determine whether the DFM object already exists
  st<-paste("DFM",id,sep="")
  found=0
  if(exists(st,where=1)) {
    data<-get(st)  
    found<-1
    if(AreParametersEqual(parameters,data$Parameters)==FALSE)
      data<-ChangeParameterObject(data,parameters)
  }
  
  ## If doesn't exist, get and create
  if(found==0) {
    
    file<-paste("DFM_",id,".csv",sep="")
    dfm<-read.csv(file,header=TRUE)  
    
    ## Get Minutes from Sample column only if ElapsedTime is not
    ## there
    if('Seconds' %in% colnames(dfm)) {
      Minutes<-dfm$Seconds/60
      dfm<-data.frame(Minutes,dfm)
    } else if(('Date' %in% colnames(dfm))&&('Time' %in% colnames(dfm))&&('MSec' %in% colnames(dfm))){
      Seconds<-GetElapsedSeconds(dfm)
      Minutes<-Seconds/60.0
      dfm<-data.frame(Minutes,Seconds,dfm)
    } else {
      stop("Time information missing from DFM data.")
    }
    if(sum(range)!=0){
      dfm<- dfm[(dfm$Minutes>range[1]) & (dfm$Minutes<range[2]),]
    }
    data=list(ID=id,Parameters=parameters,RawData=dfm)
    class(data)="DFM"
    if(!is.na(FindDataBreaks(data,multiplier=4,returnvals=FALSE))){
      cat("Data lapses found. Use FindDataBreaks for details.")
      flush.console()      
    }
    data<-CalculateBaseline(data)  
    assign(st,data,pos=1)  
  }
  data 
}

## This function will look for consecutive entries in the
## RawData$Sec column whose difference is larger than it
## should be based on the Samples.Per.Sec parameter.
FindDataBreaks<-function(dfm,multiplier=4,returnvals=TRUE){
  Interval<-diff(dfm$RawData$Seconds)
  Interval<-c(0,Interval)
  thresh<-(1.0/dfm$Parameters$Samples.Per.Second)*multiplier
  Index<-1:length(Interval)
  Index<-Index[Interval>thresh]
  Interval<-Interval[Interval>thresh]
  if(returnvals==TRUE) {
    if(length(Interval)==0)
      c(NA)
    else
      cbind(Index,Interval,dfm$RawData[Index,])  
  }
  else {
    if(length(Interval)==0)
      c(NA)
    else
      c(1)
  } 
  
}

## This function takes a vector of dates (as strings), a vector
## of times (24 hour as string) and a parameters object.
## it returns the elapsed seconds.
GetElapsedSeconds<-function(dfm){
  dates<-dfm$Date
  times<-dfm$Time
  ms <-dfm$MSec
  ## For US culture.
  tmp<-as.character(dfm$Time[1])
  tmp2<-regexpr('.M',tmp)
  if(tmp2[1]>-1) {
    ## There seems to be some confusion about the nature of the time stamp
    ## from different MCU. 
    ## Use this one if time in AM/PM
    fulltimes<-as.POSIXct(paste(dates,times),format="%m/%d/%Y %I:%M:%S %p")
  }
  else {
    ## Use this one if time is military time.
    fulltimes<-as.POSIXct(paste(dates,times),format="%m/%d/%Y %H:%M:%S")  
  }
  diffs<-as.numeric(c(difftime(fulltimes,fulltimes[1],units="secs")))
  diffs<-diffs+(ms/1000)
  diffs
}  
CalculateBaseline=function(dfm){
  window.min=dfm$Parameters$Baseline.Window.Minutes
  newData<-dfm$RawData
  # the number of samples in those minutes
  window<-window.min*60*5
  if(window %% 2 ==0) 
    window=window+1
  
  for(i in 1:12) {
    cname <-paste("W",i,sep="")
    tmp<-runmed(newData[,cname],window)
    newData[,cname]<-newData[,cname]-tmp
  }
  
  dfm$BaselineData=newData
  
  ## Now remove conflicting signals
  dfm=CleanupChamberConflicts(dfm)  
  
  ## Everything else must be recalculated
  dfm<-SetThreshold(dfm)
  dfm
}


SetThreshold = function(dfm,getStandard=TRUE) {
  ## First set the threshold...
  if(is.null(dfm$BaselineData)) {
    stop("DFM must have baseline.")
  }
  
  if(dfm$Parameters$Use.Adaptive.Threshold)  {
    if(getStandard==TRUE)
      dfm<-Set.Adaptive.Standard(dfm)    
    dfm<-Set.Adaptive.Threshold(dfm)
  }      
  else
    dfm<-Set.Fixed.Threshold(dfm)    
  
  ## Now update the licks and PI
  dfm<-Set.Feeding.Data(dfm)
  dfm<-Set.Tasting.Data(dfm)
  if(dfm$Parameters$Chamber.Size==2){
    dfm<-Set.PI.Data(dfm) 
  }
  #Other measures
  dfm<-Set.Durations.And.Intervals(dfm)
  dfm<-Set.Tasting.Durations.And.Intervals(dfm)
  dfm
  
}

Set.Feeding.Data<-function(dfm){
  if(is.null(dfm$BaselineData))
    stop("Baseline must be calculated")
  
  newData<-dfm$BaselineData
  newData2<-dfm$BaselineData
  for(i in 1:12) {
    tmp<-Set.Feeding.Data.Well(dfm,i)
    cname <-paste("W",i,sep="")
    newData[,cname]<-tmp[,1]
    newData2[,cname]<-tmp[,2]
  }
  dfm$LickData<-newData
  dfm$EventData<-newData2
  dfm  
}


Set.Feeding.Data.Well<-function(dfm,well){
  ## Get all possible feeding Licks
  thresh<-Thresholds.Well(dfm,well)
  data<-BaselinedData.Well(dfm,well)
  
  Feeding.Licks.Min<-(data > thresh$FeedingMin)
  
  Feeding.Licks.Max<-(data > thresh$FeedingMax)
  
  ## Find continguous events above min threshold with at least one value above max threshold.
  ## The result of this function is also equivalent to the Events vector
  Events<-Get.Surviving.Events(Feeding.Licks.Min,Feeding.Licks.Max)
  
  ## Now remove events that are too short
  Events[Events<dfm$Parameters$Feeding.Minevents]<-0
  
  ## Now expand the licks to TRUE/FALSE entries
  FeedingLicks<-Expand.Events(Events)
  
  data.frame(FeedingLicks,Events)
}

Set.Tasting.Data<-function(dfm){
  if(is.null(dfm$BaselineData))
    stop("Baseline must be calculated")
  if(is.null(dfm$LickData))
    stop("Feeding Licks must be calculated")
  
  newData<-dfm$BaselineData
  newData2<-dfm$BaselineData
  
  for(i in 1:12) {
    tmp<-Set.Tasting.Data.Well(dfm,i)
    cname <-paste("W",i,sep="")
    newData[,cname]<-tmp[,1]
    newData2[,cname]<-tmp[,2]
  }
  dfm$TastingData<-newData
  dfm$TastingEventData<-newData2
  dfm
}
Set.Tasting.Data.Well<-function(dfm,well){
  ## Get Tasting Licks
  ## Note that Feeding Licks have to be calculated first because if the fly is 
  ## feeding, then tasting events have to be cancelled.
  thresh<-Thresholds.Well(dfm,well)
  data<-BaselinedData.Well(dfm,well)
  
  Licks<-(data > thresh$TastingMin & 
            data < thresh$TastingMax)
  
  FeedingLicks<-FeedingData.Well.Licks(dfm,well)
  
  ## Keep only taste licks that are not feeding licks
  Licks[FeedingLicks]<-FALSE
  
  Events<-Get.Events(Licks)
  
  ## Now remove events that are too short
  Events[Events<dfm$Parameters$Tasting.Minevents]<-0
    
  data.frame(Licks,Events)
}

Set.PI.Data<-function(dfm){
  ## Get the Feeding.PI
  cnames<-paste("C",1:nrow(dfm$Parameters$Chamber.Sets),sep="")
  Minutes<-dfm$BaselineData$Minutes
  for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
    
    ## Conflicts are defined as both pads with signal greater than the
    ## minimum value of all feeding and tasting thresholds
    wellA<-dfm$Parameters$Chamber.Sets[i,1]
    wellB<-dfm$Parameters$Chamber.Sets[i,2]
    
    FeedingLicksA<-FeedingData.Well.Licks(dfm,wellA)
    FeedingLicksB<-FeedingData.Well.Licks(dfm,wellB)
    
    ## Here it is the instantaneous PI
    Feeding.PI<-FeedingLicksA - FeedingLicksB  
    
    ## Temporarily eliminate duration information for EventPI.
    tmpA<-FeedingData.Well.Events(dfm,wellA)
    tmpA[tmpA>0]<-1
    tmpB<-FeedingData.Well.Events(dfm,wellB)
    tmpB[tmpB>0]<-1
    Feeding.EventPI<-tmpA-tmpB
       
    TastingLicksA<-TastingData.Well(dfm,wellA)
    TastingLicksB<-TastingData.Well(dfm,wellB)
    
    ## Here it is the instantaneous PI
    Tasting.PI<-TastingLicksA - TastingLicksB  
   
    results<-data.frame(Minutes,Feeding.PI,Feeding.EventPI,Tasting.PI)
    names(results)<-c("Minutes","Feeding.PI", "Feeding.EventPI","Tasting.PI")
    
    if(dfm$Parameters$PI.Multiplier!=1){
      results$Feeding.PI<-results$Feeding.PI*dfm$Parameters$PI.Multiplier
      results$Feeding.EventPI<-results$Feeding.EventPI*dfm$Parameters$PI.Multiplier
      results$Tasting.PI<-results$Tasting.PI*dfm$Parameters$PI.Multiplier
    }
    
    if(i==1){    
      PIData=list(C1=results)      
    }
    else {
      s<-paste("C",i,sep="")      
      PIData[[s]]<-results    
    }        
  }
  row.names(PIData)<-NULL
  dfm$PIData<-PIData
  dfm
}

## This new function uses a new parameter, Signal.Threshold,
## to remove positive signals that conflict.  The higher signal
## is kept.  The lower one is set to baseline.
CleanupChamberConflicts<-function(dfm){
  ## This function normally takes baselined data.
  
  if(is.null(dfm$BaselineData))
    stop("Baseline must be calculated")
  
  ## Note that we don't need to do anything if the chamber size is 1
  ## because there is no conflict by definition.
  
  if(dfm$Parameters$Chamber.Size==2) {
    cat("\n")
    flush.console()
    for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
      wellA<-dfm$Parameters$Chamber.Sets[i,1]
      wellB<-dfm$Parameters$Chamber.Sets[i,2]
      dataA<-BaselinedData.Well(dfm,wellA)
      dataB<-BaselinedData.Well(dfm,wellB)
      
      signalA<-(dataA>dfm$Parameters$Signal.Threshold)
      signalB<-(dataB>dfm$Parameters$Signal.Threshold)
      
      awins<-dataA>dataB
      bwins<-dataB>dataA
      conflicts<-0
      
      #Clean the feeding vectors
      conflicts<-sum(signalA & signalB)
      
      ## conflict resolution involves accepting the plate with the larger value
      ## and setting the other to baseline.
      ## cat("DFM: ",dfm$ID," Chamber:",i," Cleaning ",conflicts," conflicts.\n")
      flush.console()
      
      if(conflicts>0) {   
        dataA[(signalA & signalB & bwins)]<-0
        dataB[(signalA & signalB & awins)]<-0
        
        ## Correct the data
        cname <-paste("W",wellA,sep="")
        dfm$BaselineData[,cname]<-dataA
        
        cname <-paste("W",wellB,sep="")
        dfm$BaselineData[,cname]<-dataB
      }
    }
  }
  if(dfm$Parameters$Chamber.Size>2) {
    stop("Clean chambers not implemented for chamber size >2.")    
  }
  
  dfm
}

## This depricated function does not change the baselined data
## it now only alters the feeding and tasting licks 
## to ensure that single flies can not feed from both
## simultaneously. It will replace feeding and tasting data.
CleanupChamberConflictsOLD<-function(dfm){
  ## This function normally takes baselined data.
  
  if(is.null(dfm$LickData))
    stop("Feeding Lick Data must be calculated")
  
  if(is.null(dfm$TastingData))
    stop("TastingData must be calculated")
  
  ## Note that we don't need to do anything if the chamber size is 1
  ## because there is no conflict by definition.
  
  if(dfm$Parameters$Chamber.Size==2) {
    cat("\n")
    flush.console()
    for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
      wellA<-dfm$Parameters$Chamber.Sets[i,1]
      wellB<-dfm$Parameters$Chamber.Sets[i,2]
      dataA<-BaselinedData.Well(dfm,wellA)
      dataB<-BaselinedData.Well(dfm,wellB)
      
      feedingA<-FeedingData.Well.Licks(dfm,wellA)
      feedingB<-FeedingData.Well.Licks(dfm,wellB)
      
      tastingA<-TastingData.Well(dfm,wellA)
      tastingB<-TastingData.Well(dfm,wellB)
      
      awins<-dataA>dataB
      bwins<-dataB>dataA
      conflicts<-0
      
      #Clean the feeding vectors
      conflicts<-conflicts+sum(feedingA & feedingB & bwins)+sum(feedingA & feedingB & awins)
     
      
      #Clean the tasting vectors
      conflicts<-conflicts+sum(feedingB & tastingA)+sum(feedingA & tastingB)
    
      conflicts<-conflicts+sum(tastingB & tastingA & bwins)+sum(tastingB & tastingA & awins)
           
      ## conflict resolution involves accepting the plate with the larger value
      ## and setting the other to baseline.
      cat("DFM: ",dfm$ID," Chamber:",i," Cleaning ",conflicts," conflicts.\n")
      flush.console()
      
      if(conflicts>0) {   
        feedingA[(feedingA & feedingB & bwins)]<-FALSE
        feedingB[(feedingA & feedingB & awins)]<-FALSE
        tastingA[(feedingB & tastingA)]<-FALSE
        tastingB[(feedingA & tastingB)]<-FALSE
        tastingA[(tastingB & tastingA & bwins)]<-FALSE
        tastingB[(tastingB & tastingA & awins)]<-FALSE
        
        ## Correct the feeding and tasting entries
        cname <-paste("W",wellA,sep="")
        dfm$FeedingData[,cname]<-feedingA
        dfm$TastingData[,cname]<-tastingA
      
        cname <-paste("W",wellB,sep="")
        dfm$FeedingData[,cname]<-feedingB
        dfm$TastingData[,cname]<-tastingB
      }
    }
  }
  if(dfm$Parameters$Chamber.Size>2) {
    stop("Clean chambers not implements for chamber size >2.")    
  }
  
  dfm
}

Set.Adaptive.Standard<-function(dfm){
  stand<-Set.Adaptive.Standard.Well(dfm,1)
  for(i in 2:12){
    tmp<-Set.Adaptive.Standard.Well(dfm,i)
    stand<-cbind(stand,tmp)
    
  }
  AdaptiveStandard<-data.frame(stand)
  names(AdaptiveStandard)<-paste("W",1:12,sep="")
  dfm$AdapativeStandard<-AdaptiveStandard
  dfm
}

Set.Adaptive.Standard.Well<-function(dfm,well){ 
  sps<-dfm$Parameters$Samples.Per.Sec
  data<-BaselinedData.Well(dfm,well)
  Standard.thresh<-rep(-1,length(data))
  
  ## Note that window.size will be the complete size
  ## (two-sided) of the window.
  window.size<-dfm$Parameters$Adaptive.Threshold.Window.Minutes*60*sps
  
  if(window.size %%2 == 0)
    window.size<-window.size+1
  
  window.arm<-(window.size-1)/2
  mA<-length(data)
  
  sq<-dfm$Parameters$Adaptive.Threshold.Selection.Quan
  
  for(i in 1:mA){
    lindex<-max(1,(i-window.arm))
    hindex<-min(mA,(i+window.arm))
    
    Standard.thresh[i]<-quantile(data[lindex:hindex],sq)        
    
    if(i%%10000==0) {
      print(paste(i,"of",mA,"in well",well))
      flush.console()
    }
  } 
  
  Standard.thresh
}

set.Adaptive.Threshold<-function(dfm){
  if(is.null(dfm$AdaptiveStandard)) {
    stop("DFM must have standard.")
  }
  tmp<-Set.Adaptive.Threshold.Well(dfm,1)
  Thresholds$W1<-tmp
  
  for(i in 2:12){
    s<-paste("W",i,sep="")
    tmp<-Set.Adaptive.Threshold.Well(dfm,i)
    Thresholds[[s]]<-tmp    
  }
  dfm$Thresholds<-Thresholds
  dfm  
}

Set.Adaptive.Threshold.Well<-function(dfm,well){   
  cname<-paste("W",well,sep="")
  stand<-dfm$AdaptiveStandard[,cname]
  feeding.max.thresh<-chamber$Parameters$Feeding.Threshold.Value*stand
  feeding.min.thresh<-chamber$Parameters$Feeding.Interval.Minimum*stand
  tasting.max.thresh<-chamber$Parameters$Tasting.Threshold.Interval.Low*stand
  tasting.min.thresh<-chamber$Parameters$Tasting.Threshold.Interval.High*stand
  
  min.thresh<-chamber$Parameters$Adaptive.Threshold.Minimum
  feeding.max.thresh[feeding.max.thresh<min.thresh]<-min.thresh
  feeding.min.thresh[feeding.min.thresh<min.thresh]<-min.thresh
  tasting.max.thresh[tasting.max.thresh<min.thresh]<-min.thresh
  tasting.min.thresh[tasting.min.thresh<min.thresh]<-min.thresh
  
  r.tmp<-data.frame(feeding.max.thresh,feeding.min.thresh,tasting.max.thresh,tasting.min.thresh)
  names(r.tmp)<-c("FeedingMax","FeedingMin","TastingMax","TastingMin")                    
  r.tmp
}

Set.Fixed.Threshold<-function(dfm){
  tmp<-Set.Fixed.Threshold.Well(dfm,1)
  Thresholds=list(W1=tmp)
  
  for(i in 2:12){
    s<-paste("W",i,sep="")
    tmp<-Set.Fixed.Threshold.Well(dfm,i)
    Thresholds[[s]]<-tmp    
  }
  dfm$Thresholds<-Thresholds
  dfm  
}

Set.Fixed.Threshold.Well<-function(dfm,well){
  n<-SampleCount(dfm)
  ## Get well specific thresholds if the values are < 0 
  if(dfm$Parameters$Feeding.Threshold.Value<0){
    ## Find maximum reading
    tmp<-max(BaselinedData.Well(dfm,well))
    tmpA <- round(tmp*abs(dfm$Parameters$Feeding.Threshold.Value),0)
    tmpB <- round(tmp*abs(dfm$Parameters$Feeding.Interval.Minimum),0)
    tmpC <- round(tmp*abs(dfm$Parameters$Tasting.Threshold.Interval.Low),0)
    tmpD <-round(tmp*abs(dfm$Parameters$Tasting.Threshold.Interval.High),0)
  }
  else {
    tmpA<-dfm$Parameters$Feeding.Threshold.Value 
    tmpB<-dfm$Parameters$Feeding.Interval.Minimum
    tmpC<-dfm$Parameters$Tasting.Threshold.Interval.Low
    tmpD<-dfm$Parameters$Tasting.Threshold.Interval.High
  }
  
  feeding.max.thresh<-rep(tmpA,n)
  feeding.min.thresh<-rep(tmpB,n)
  tasting.min.thresh<-rep(tmpC,n)
  tasting.max.thresh<-rep(tmpD,n)
  
  
  r.tmp<-data.frame(feeding.max.thresh,feeding.min.thresh,tasting.max.thresh,tasting.min.thresh)       
  
  names(r.tmp)<-c("FeedingMax","FeedingMin","TastingMax","TastingMin")                            
  r.tmp  
}


Set.Durations.And.Intervals<-function(dfm){
  tmp<-Set.Durations.And.Intervals.Well(dfm,1)
  Durations = list(W1=tmp$Durations)
  Intervals = list(W1=tmp$Intervals)
  
  for(i in 2:12){
    s<-paste("W",i,sep="")
    tmp<-Set.Durations.And.Intervals.Well(dfm,i)
    Durations[[s]]<-tmp$Durations  
    Intervals[[s]]<-tmp$Intervals
  }
  
  dfm$Durations<-Durations
  dfm$Intervals<-Intervals
  dfm
}

Set.Durations.And.Intervals.Well<-function(dfm,well){
  data<-BaselineData.Well(dfm,well)
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
    for(i in 1:length(indices)){
      dataindex<-indices[i]
      eventlength<-boutDurs[i]
      tmp2<-data[dataindex:(dataindex+(eventlength-1))]
      max.inten[i]<-max(tmp2)
      min.inten[i]<-min(tmp2)
      sum.inten[i]<-sum(tmp2)
      avg.inten[i]<-mean(tmp2)  
      var.inten[i]<-var(tmp2)
    }
    
    BoutData<-data.frame(min.inten,max.inten,sum.inten,avg.inten,var.inten)
    names(BoutData)<-c("MinIntensity","MaxIntensity","SumIntensity","MeanIntensity","VarIntensity")
    
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
    Durations<-data.frame(Minutes,Events,Duration,SumInten,AvgInten,MinInten,MaxInten,VarInten)
    names(Durations)<-c("Minutes","Licks","Duration","TotalIntensity","AvgIntensity","MinIntensity","MaxIntensity","VarIntensity")    
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

Set.Tasting.Durations.And.Intervals<-function(dfm){
  tmp<-Set.Tasting.Durations.And.Intervals.Well(dfm,1)
  Durations = list(W1=tmp$Durations)
  Intervals = list(W1=tmp$Intervals)
  
  for(i in 2:12){
    s<-paste("W",i,sep="")
    tmp<-Set.Tasting.Durations.And.Intervals.Well(dfm,i)
    Durations[[s]]<-tmp$Durations  
    Intervals[[s]]<-tmp$Intervals
  }
  
  dfm$TastingDurations<-Durations
  dfm$TastingIntervals<-Intervals
  dfm
}

Set.Tasting.Durations.And.Intervals.Well<-function(dfm,well){
  data<-BaselineData.Well(dfm,well)
  events<-TastingData.Well.Events(dfm,well)
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
    for(i in 1:length(indices)){
      dataindex<-indices[i]
      eventlength<-boutDurs[i]
      tmp2<-data[dataindex:(dataindex+(eventlength-1))]
      max.inten[i]<-max(tmp2)
      min.inten[i]<-min(tmp2)
      sum.inten[i]<-sum(tmp2)
      avg.inten[i]<-mean(tmp2)  
      var.inten[i]<-var(tmp2)
    }
    
    BoutData<-data.frame(min.inten,max.inten,sum.inten,avg.inten,var.inten)
    names(BoutData)<-c("MinIntensity","MaxIntensity","SumIntensity","MeanIntensity","VarIntensity")
    
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
    Durations<-data.frame(Minutes,Events,Duration,SumInten,AvgInten,MinInten,MaxInten,VarInten)
    names(Durations)<-c("Minutes","Licks","Duration","TotalIntensity","AvgIntensity","MinIntensity","MaxIntensity","VarIntensity")    
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

Thresholds.Well<-function(dfm,well,range=c(0,0)){
  cname=paste("W",well,sep="")
  tmp<-dfm$Thresholds[[cname]]
  if(sum(range)!=0) {
    tmp<- tmp[(dfm$BaselineData$Minutes>range[1]) & (dfm$BaselineData$Minutes<range[2]),]
  }    
  tmp  
}
BaselinedData.Well<-function(dfm,well,range=c(0,0)) {  
  cname=paste("W",well,sep="")
  tmp<-dfm$BaselineData[,cname]  
  if(sum(range)!=0) {
    tmp<- tmp[(dfm$BaselineData$Minutes>range[1]) & (dfm$BaselineData$Minutes<range[2])]
  }    
  tmp  
}
BaselinedData<-function(dfm,range=c(0,0)) {   
  tmp<-dfm$BaselineData
  if(sum(range)!=0) {
    tmp<- tmp[(dfm$BaselineData$Minutes>range[1]) & (dfm$BaselineData$Minutes<range[2]),]
  }    
  tmp  
}
SampleCount<-function(dfm,range=c(0,0)){
  nrow(BaselinedData(dfm,range))  
}
FeedingData.Well.Licks<-function(dfm,well,range=c(0,0)){
  cname=paste("W",well,sep="")
  tmp<-FeedingData.Licks(dfm,range)
  tmp[,cname]  
}
## Remember that this function returns a vector with 
## duration of event information as well.
## Need to set these to 1 to get number of events.
FeedingData.Well.Events<-function(dfm,well,range=c(0,0)){
  cname=paste("W",well,sep="")
  tmp<-FeedingData.Events(dfm,range)
  tmp[,cname]  
}
TastingData.Well<-function(dfm,well,range=c(0,0)){
  cname=paste("W",well,sep="")
  tmp<-dfm$TastingData[,cname]  
  if(sum(range)!=0) {
    tmp<- tmp[(tmp$Minutes>range[1]) & (tmp$Minutes<range[2])]
  }   
  tmp    
}
TastingData.Well.Events<-function(dfm,well,range=c(0,0)){
  cname=paste("W",well,sep="")
  tmp<-TastingData.Events(dfm,range)
  tmp[,cname]  
}
TastingData.Events<-function(dfm,range=c(0,0)){
  data<-dfm$TastingEventData  
  if(sum(range)!=0) {
    data<- data[(data$Minutes>range[1] & data$Minutes<range[2]),]
  }    
  data
}
FeedingData.Licks<-function(dfm,range=c(0,0)){
  data<-dfm$LickData  
  if(sum(range)!=0) {
    data<- data[(data$Minutes>range[1] & data$Minutes<range[2]),]
  }    
  data
}
## Remember that this function returns a vector with 
## duration of event information as well.
## Need to set these to 1 to get number of events.
FeedingData.Events<-function(dfm,range=c(0,0)){
  data<-dfm$EventData  
  if(sum(range)!=0) {
    data<- data[(data$Minutes>range[1] & data$Minutes<range[2]),]
  }    
  data
}
TastingData<-function(dfm,range=c(0,0)){
  data<-dfm$TastingData  
  if(sum(range)!=0) {
    data<- data[(data$Minutes>range[1] & data$Minutes<range[2]),]
  }    
  data
}
Feeding.TotalLicks<-function(dfm,range=c(0,0)){
  result<-rep(-1,12)
  data<-FeedingData.Licks(dfm,range)
  for(i in 1:12) {
    cname=paste("W",i,sep="")
    tmp<-data[,cname]
    result[i]<-sum(tmp)
  }
  names(result)<-paste("W",1:12,sep="")
  result
}
Feeding.TotalLicks.Well<-function(dfm,well,range=c(0,0)){
  tmp<-Feeding.TotalLicks(dfm,range)
  tmp[well]
}
Feeding.TotalEvents<-function(dfm,range=c(0,0)){
  result<-rep(-1,12)
  data<-FeedingData.Events(dfm,range)
  for(i in 1:12) {
    cname=paste("W",i,sep="")
    tmp<-data[,cname]
    result[i]<-sum(tmp>0)
  }
  names(result)<-paste("W",1:12,sep="")
  result  
}
Feeding.TotalEvents.Well<-function(dfm,well,range=c(0,0)){
  tmp<-Feeding.TotalEvents(dfm,range)
  tmp[well]
}
Tasting.TotalLicks<-function(dfm,range=c(0,0)){
  result<-rep(-1,12)
  data<-TastingData(dfm,range)
  for(i in 1:12) {
    cname=paste("W",i,sep="")
    tmp<-data[,cname]
    result[i]<-sum(tmp)
  }
  names(result)<-paste("W",1:12,sep="")
  result
}

Tasting.TotalLicks.Well<-function(dfm,well,range=c(0,0)){
  tmp<-Tasting.TotalLicks(dfm,range)
  tmp[well]
}

BaselineData<-function(dfm,range=c(0,0)){
  tmp<-dfm$BaselineData
  if(sum(range)!=0) {
    tmp<- tmp[(tmp$Minutes>range[1]) & (tmp$Minutes<range[2]),]
  }    
  tmp  
}

BaselineData.Well=function(dfm,well,range=c(0,0)) {
  cname=paste("W",well,sep="")
  tmp<-BaselineData(dfm,range)
  tmp[,cname]
}
RawData=function(dfm,range=c(0,0)) {
  tmp<-dfm$RawData
  if(sum(range)!=0) {
    tmp<- tmp[(tmp$Minutes>range[1]) & (tmp$Minutes<range[2]),]
  }    
  tmp
}

Feeding.IntervalSummary.Well<-function(dfm,well,range=c(0,0)){
  cname=paste("W",well,sep="")
  adurs<-dfm$Intervals[[cname]]
  if(sum(range)!=0){
    if(!is.data.frame(adurs)){
      a<-0
      aa<-0
    }
    else {
      adurs<-adurs[(adurs$Minutes>range[1]) & (adurs$Minutes<range[2]),]  
      if(nrow(adurs)==0){
        a<-0
        aa<-0
      }
      else {    
        a<-mean(adurs$IntervalSec)
        aa<-median(adurs$IntervalSec)  
      }
    }  
  }
  else {
    if(!is.data.frame(adurs)){
      a<-0
      aa<-0
    } else {
      a<-mean(adurs$IntervalSec)  
      aa<-median(adurs$IntervalSec)
    }
  }
  
  if(is.na(a)||is.nan(a)) a<-0
  if(is.na(aa)||is.nan(aa)) aa<-0
  tmp<-data.frame(a,aa)
  names(tmp)<-c("MeanTimeBtw","MedTimeBtw")
  tmp
}
Feeding.DurationSummary.Well<-function(dfm,well,range=c(0,0)){
  cname=paste("W",well,sep="")
  adurs<-dfm$Durations[[cname]]
  if(sum(range)!=0){
    if(!is.data.frame(adurs)){
      a<-0
      aa<-0
    }
    else {
      adurs<-adurs[(adurs$Minutes>range[1]) & (adurs$Minutes<range[2]),]  
      if(nrow(adurs)==0){
        a<-0
        aa<-0
      }
      else {    
        a<-mean(adurs$Duration)
        aa<-median(adurs$Duration)  
      }
    }
  }
  else {
    if(!is.data.frame(adurs)){
      a<-0
      aa<-0
    } else {
      a<-mean(adurs$Duration)  
      aa<-median(adurs$Duration)
    }
  }
  
  if(is.na(a)||is.nan(a)) a<-0
  if(is.na(aa)||is.nan(aa)) aa<-0
  tmp<-data.frame(a,aa)
  
  names(tmp)<-c("MeanDur","MedianDur")
  tmp
}
Feeding.IntensitySummary.Well<-function(dfm,well,range=c(0,0)){
  d<-BaselineData.Well(dfm,well,range)
  l<-FeedingData.Well.Licks(dfm,well,range)
  
  da<-d[l]
  
  if(length(da)==0){
    a<-0
    aa<-0
  }
  else {
    a<-mean(da)  
    aa<-median(da)
  }
    
  tmp<-data.frame(a,aa)
  names(tmp)<-c("MeanInt","MedianInt")
  tmp
}

IsThresholdAdaptive<-function(dfm) {
  dfm$Parameters$Use.Adaptive.Threshold
}

BaselinedData.Range.Well<-function(dfm,well,range=c(0,0)){
  tmp<-BaselinedData.Well(dfm,well,range)
  x1<-min(tmp)
  x2<-max(tmp)
  c(x1,x2)
}
Minutes<-function(dfm) {
  dfm$BaselineData$Minutes
}


Feeding.Durations.Well<-function(dfm,well){
  cname=paste("W",well,sep="")
  adurs<-dfm$Durations[[cname]]
  adurs
}
Feeding.Intervals.Well<-function(dfm,well){
  cname=paste("W",well,sep="")
  adurs<-dfm$Intervals[[cname]]
  adurs
}

LastSampleData.Well<-function(dfm,well){
  tmp<-BaselinedData.Well(dfm,well)
  tmp[length(tmp)]
}
FirstSampleData.Well<-function(dfm,well){
  tmp<-BaselinedData.Well(dfm,well)  
  tmp[1]
}

LastSampleData<-function(dfm){
  tmp<-BaselinedData(dfm)
  nr<-nrow(tmp)
  tmp[nr,]
}
FirstSampleData<-function(dfm){
  tmp<-BaselinedData(dfm)  
  tmp[1,]
}

#########################
## Utilities

## This function takes 2 vectors, one with the events
## above a minimal threshold (minvec) and one that
## specifies events that pass a more stringent threshold (maxvec).
## Contiguous events are only kept if at least one
## value in the event, which is defined by minvec, is above
## the higher threshold, which is defined by max vec
## z <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
## zz <- c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)
## Get.Surviving.Events(z,zz) -> (2 0 0 0 0 0 3 0 0)
Get.Surviving.Events<-function(minvec,maxvec){
  tmp<-Get.Events(minvec)
  result<-tmp
  indices<-(1:length(minvec))[tmp>0]
  for(i in indices){
    tmp2<-maxvec[i:(i+(tmp[i]-1))]
    if(sum(tmp2)==0)
      result[i]<-0  
  }
  result
}


## This function is the reverse of Get.Events
## (2 0 0 0 1 0 3 0 0) -> c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
Expand.Events<-function(eventvec){
  result<-rep(FALSE,length(eventvec))
  indices<-(1:length(eventvec))[eventvec>0]
  for(i in indices){
    result[i:(i+eventvec[i]-1)]<-TRUE   
  }
  result
}

## These functions are helper functions for the basic calculations
# This function replaces continuing events with zero and make the first event of that
# episode equal to its duration.
## c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE) -> (2 0 0 0 1 0 3 0 0)
Get.Events<-function(z){
  tmp<-rle(z)
  result<-c(-1)
  for(i in 1:length(tmp$lengths)){
    if(tmp$values[i]){
      tmp2<-c(tmp$lengths[i],rep(0,tmp$lengths[i]-1))
      result<-c(result,tmp2)
    }
    else {
      tmp2<-c(rep(0,tmp$lengths[i]))
      result<-c(result,tmp2)
    }
  }
  result[-1]
}
Get.Events.And.Intensities<-function(z,data){
  z<-Get.Events(z)
  max.inten<-rep(0,length(z))
  min.inten<-rep(0,length(z))
  sum.inten<-rep(0,length(z))
  avg.inten<-rep(0,length(z))
  
  indices<-(1:length(z))[z>0]
  for(i in indices){
    tmp2<-data[i:(i+(z[i]-1))]
    max.inten[i]<-max(tmp2)
    min.inten[i]<-min(tmp2)
    sum.inten[i]<-sum(tmp2)
    avg.inten[i]<-mean(tmp2)    
  }
  result<-data.frame(z,min.inten,max.inten,sum.inten,avg.inten)
  names(result)<-c("FeedingEvent","MinIntensity","MaxIntensity","SumIntensity","MeanIntensity")
  result
}
# This function replaces continuing events with zero and make the first event of that
# episode equal to its duration.
## c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE) -> (0 0 2 0 0 1 0 0 0)
Get.Intervals<-function(z){
  tmp<-rle(z)
  result<-c(-1)
  for(i in 1:length(tmp$lengths)){
    if(!tmp$values[i]){
      tmp2<-c(tmp$lengths[i],rep(0,tmp$lengths[i]-1))
      result<-c(result,tmp2)
    }
    else {
      tmp2<-c(rep(0,tmp$lengths[i]))
      result<-c(result,tmp2)
    }
  }
  result[-1]
}

CleanDFM<-function(){
  tmp<-ls(pattern="DFM.",pos=1)
  rm(list=tmp,pos=1)
  tmp<-ls(pattern="DFM..",pos=1)
  rm(list=tmp,pos=1)
}
UpdateHiddenDFMObject<-function(dfm){
  st<-paste("DFM",dfm$ID,sep="")
  assign(st,dfm,pos=1)
}
GetDFMParameterVector<-function(dfm){
  GetParameterVector(dfm$Parameters)  
}

ChangeParameterObject<-function(dfm,newP) {
  p<-dfm$Parameters
  baseline.flag<-FALSE
  threshold.flag<-FALSE
  adaptive.baseline.flag<-FALSE
  eventpi.flag<-FALSE
  tmp.O<-options()
  options(warn=-1)
  dfm$Parameters<-newP
  ## Change only those that are listed
  if(p$Baseline.Window.Minutes!=newP$Baseline.Window.Minutes) {    
    baseline.flag<-TRUE
  }
  if(p$Feeding.Threshold.Value!=newP$Feeding.Threshold.Value) {    
    threshold.flag<-TRUE
  }
  if(p$Feeding.Interval.Minimum!=newP$Feeding.Interval.Minimum) {    
    threshold.flag<-TRUE
  }
  if(p$Tasting.Threshold.Interval.Low!=newP$Tasting.Threshold.Interval.Low) {    
    threshold.flag<-TRUE
  }
  if(p$Tasting.Threshold.Interval.High!=newP$Tasting.Threshold.Interval.High) {    
    threshold.flag<-TRUE
  }
  if(p$Adaptive.Threshold.Minimum!=newP$Adaptive.Threshold.Minimum){
    threshold.flag<-TRUE
  }
  if(p$Adaptive.Threshold.Window.Minutes!=newP$Adaptive.Threshold.Window.Minutes){    
    adaptive.baseline.flag<-TRUE
  }
  if(p$Adaptive.Threshold.Selection.Quant!=newP$Adaptive.Threshold.Selection.Quant){    
    adaptive.baseline.flag<-TRUE
  }
  if(p$Use.Adaptive.Threshold!=newP$Use.Adaptive.Threshold){    
    adaptive.baseline.flag<-TRUE
  }
  if(p$Feeding.Minevents!=newP$Feeding.Minevents){
    eventpi.flag<-TRUE
  }
  if(p$Tasting.Minevents!=newP$Tasting.Minevents){
    eventpi.flag<-TRUE
  }
  if(p$Samples.Per.Second!=newP$Samples.Per.Second){
    adaptive.baseline.flag<-TRUE
  }
  if(p$Chamber.Size !=newP$Chamber.Size){
    baseline.flag<-TRUE
  }
  if(sum(c(p$Chamber.Sets)!=c(newP$Chamber.Sets))!=0){
    baseline.flag<-TRUE
  }
  if(p$Signal.Threshold!=newP$Signal.Threshold){
    baseline.flag<-TRUE
  }
  
  if(p$PI.Multiplier!=newP$PI.Multiplier){
    eventpi.flag<-TRUE
  }
  
  ## Now update the stats needed
  if(baseline.flag==TRUE) {
    dfm<-CalculateBaseline(dfm)
  }
  else if(adaptive.baseline.flag==TRUE){
    dfm<-SetThreshold(dfm)
  }
  else if(threshold.flag==TRUE) {
    dfm<-SetThreshold(dfm,getStandard=FALSE)
  }
  else if(eventpi.flag==TRUE) {
    dfm<-Set.Feeding.Data(dfm)
    dfm<-Set.Tasting.Data(dfm)
    if(dfm$Parameters$Chamber.Size==2){
      dfm<-Set.PI.Data(dfm) 
    }
    dfm<-Set.Durations.And.Intervals(dfm)
    dfm<-Set.Tasting.Durations.And.Intervals(dfm)
  }
  options(tmp.O)
  UpdateHiddenDFMObject(dfm)
  dfm
}

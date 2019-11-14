####
## These are essentially private functions used to calculate DFM data
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
  ## Everything else must be recalculated
  dfm<-SetThreshold(dfm)
  dfm
}
SetThreshold = function(dfm,getStandard=TRUE) {
  ## First set the threshold...
  if(is.null(dfm$BaselineData)) {
    stop("DFM must have baseline.")
  }
  
  dfm<-Set.Fixed.Threshold(dfm)    
  
  ## Now update the licks and PI
  dfm<-Set.Feeding.Data(dfm)
  dfm<-Set.Tasting.Data(dfm)
  
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
  if(dfm$Parameters$Feeding.Threshold<0){
    ## Find maximum reading
    tmp<-max(BaselinedData.Well(dfm,well))
    tmpA <- round(tmp*abs(dfm$Parameters$Feeding.Threshold),0)
    tmpB <- round(tmp*abs(dfm$Parameters$Feeding.Minimum),0)
    tmpC <- round(tmp*abs(dfm$Parameters$Tasting.Minimum),0)
    tmpD <-round(tmp*abs(dfm$Parameters$Tasting.Maximum),0)
  }
  else {
    tmpA<-dfm$Parameters$Feeding.Threshold 
    tmpB<-dfm$Parameters$Feeding.Minimum
    tmpC<-dfm$Parameters$Tasting.Minimum
    tmpD<-dfm$Parameters$Tasting.Maximum
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
BaselineData.Well=function(dfm,well,range=c(0,0)) {
  cname=paste("W",well,sep="")
  tmp<-BaselineData(dfm,range)
  tmp[,cname]
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
BaselinedData.Range.Well<-function(dfm,well,range=c(0,0)){
  tmp<-BaselinedData.Well(dfm,well,range)
  x1<-min(tmp)
  x2<-max(tmp)
  c(x1,x2)
}
Minutes<-function(dfm,range=c(0,0)) {
  data<-dfm$BaselineData$Minutes
  if(sum(range)!=0) {
    data<- data[(data>range[1] & data<range[2])]
  }    
  data
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


UpdateHiddenDFMObject<-function(dfm){
  st<-paste("DFM",dfm$ID,sep="")
  assign(st,dfm,pos=1)
}
GetDFMParameterVector<-function(dfm){
  GetParameterVector(dfm$Parameters)  
}

## Private Functions For the Common Chamber Class

Feeding.Summary.OneWell<-function(dfm,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for single chambers only")
  
  for(i in 1:12 ){
    interval<-Feeding.IntervalSummary.Well(dfm,i,range)
    intensity<-Feeding.IntensitySummary.Well(dfm,i,range)
    dur<-Feeding.DurationSummary.Well(dfm,i,range)  
    FLicks<-Feeding.TotalLicks.Well(dfm,i,range)
    FEvents<-Feeding.TotalEvents.Well(dfm,i,range)    
    if(i==1)
      result<-data.frame(matrix(c(dfm$ID,i,FLicks,FEvents,unlist(dur),unlist(interval),unlist(intensity),range[1],range[2]),nrow=1))  
    else {
      tmp<-data.frame(matrix(c(dfm$ID,i,FLicks,FEvents,unlist(dur),unlist(interval),unlist(intensity),range[1],range[2]),nrow=1))  
      result<-rbind(result,tmp)
    }      
  }
  names(result)<-c("DFM","Chamber","Licks","Events","MeanDuration","MedDuration",
                   "MeanTimeBtw","MedTimeBtw","MeanInt","MedianInt","StartMin","EndMin")
  if(TransformLicks==TRUE)
    result$Licks<-result$Licks^0.25
  result    
  
}
Feeding.Summary.TwoWell<-function(dfm,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
    if(dfm$Parameters$PI.Multiplier==1){
      wellA<-dfm$Parameters$Chamber.Sets[i,1]
      wellB<-dfm$Parameters$Chamber.Sets[i,2] 
    }
    else {
      wellB<-dfm$Parameters$Chamber.Sets[i,1]
      wellA<-dfm$Parameters$Chamber.Sets[i,2] 
    }
    
    interval.a<-Feeding.IntervalSummary.Well(dfm,wellA,range)
    intensity.a<-Feeding.IntensitySummary.Well(dfm,wellA,range)
    dur.a<-Feeding.DurationSummary.Well(dfm,wellA,range)  
    FLicks.a<-Feeding.TotalLicks.Well(dfm,wellA,range)
    FEvents.a<-Feeding.TotalEvents.Well(dfm,wellA,range)    
    
    interval.b<-Feeding.IntervalSummary.Well(dfm,wellB,range)
    intensity.b<-Feeding.IntensitySummary.Well(dfm,wellB,range)
    dur.b<-Feeding.DurationSummary.Well(dfm,wellB,range)  
    FLicks.b<-Feeding.TotalLicks.Well(dfm,wellB,range)
    FEvents.b<-Feeding.TotalEvents.Well(dfm,wellB,range) 
    
    FPIs<-c((FLicks.a-FLicks.b)/(FLicks.a+FLicks.b),(FEvents.a-FEvents.b)/(FEvents.a+FEvents.b))
    if(i==1){
      result<-data.frame(matrix(c(dfm$ID,i,FPIs,FLicks.a,FLicks.b,FEvents.a,FEvents.b,unlist(dur.a),unlist(dur.b),unlist(interval.a),
                                  unlist(interval.b),unlist(intensity.a),unlist(intensity.b),range[1],range[2]),nrow=1))    
      
    }
    else {
      tmp<-data.frame(matrix(c(dfm$ID,i,FPIs,FLicks.a,FLicks.b,FEvents.a,FEvents.b,unlist(dur.a),unlist(dur.b),unlist(interval.a),
                               unlist(interval.b),unlist(intensity.a),unlist(intensity.b),range[1],range[2]),nrow=1))
      result<-rbind(result,tmp)      
    }
  }
  names(result)<-c("DFM","Chamber","PI","EventPI","LicksA","LicksB","EventsA","EventsB","MeanDurationA","MedDurationA",
                   "MeanDurationB","MedDurationB","MeanTimeBtwA","MedTimeBtwA",
                   "MeanTimeBtwB","MedTimeBtwB","MeanIntA","MedianIntA",
                   "MeanIntB","MedianIntB","StartMin","EndMin")
  if(TransformLicks==TRUE){
    result$LicksA<-result$LicksA^0.25
    result$LicksB<-result$LicksB^0.25
  }
  result    
  
}
FeedingLicks.OneWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE,TransformLicks=TRUE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingLicksBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      dfm<-GetDFM(monitors[1])
      if(is.na(dfm))
        stop("DFM Missing")
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE,TransformLicks)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    if(TransformLicks==TRUE){
      r<-paste("Transformed Licks -- Range(min): (",range[1],",",range[2],")",sep="")
      ylabel<-"Transformed Licks"
    }
    else {
      r<-paste("Licks -- Range(min): (",range[1],",",range[2],")",sep="")
      ylabel<-"Licks"
    }
    
    print(ggplot(results, aes(results$Treatment, results$Licks)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Licks),max(results$Licks))) + ggtitle(r) + xlab("Treatment") +ylab(ylabel) + guides(fill=FALSE))
    r2<-paste("\n** ",r," **\n")
    cat(r2)
    print(summary(aov(Licks~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE,TransformLicks)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        if(TransformLicks==TRUE){
          r<-paste("Transformed Licks -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
          ylabel<-"Transformed Licks"
        }
        else {
          r<-paste("Licks -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
          ylabel<-"Licks"
        }
        r2<-paste("\n** ",r," **\n")
        cat(r2)
        print(summary(aov(Licks~Treatment,data=results)))
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Licks)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Licks),max(results$Licks))) + ggtitle(r) + xlab("Treatment") +ylab(ylabel) + guides(fill=FALSE))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
  
} 
FeedingEvents.OneWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingEventsBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      dfm<-GetDFM(monitors[1])
      if(is.na(dfm))
        stop("DFM Missing")
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    r<-paste("Events-Range: (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Events") + guides(fill=FALSE))
    cat("** ANOVA results Full Range **\n\n")
    print(summary(aov(Events~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("Events-Range: (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
        ppp<-paste("\n** ANOVA results -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        cat(ppp)
        print(summary(aov(Events~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
} 
FeedingLicks.TwoWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE,TransformLicks=TRUE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingLicksBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      dfm<-GetDFM(monitors[1])
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE,TransformLicks)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    Licks<-results$LicksA+results$LicksB
    if(TransformLicks==TRUE){
      r<-paste("Transformed Total Licks -- Range(min): (",range[1],",",range[2],")",sep="")
      ylabel<-"Transformed Total Licks (A+B)"
    }
    else {
      r<-paste("Licks -- Range(min): (",range[1],",",range[2],")",sep="")
      ylabel<-"Total Licks (A+B)"
    }
    results<-data.frame(results,Licks)
    r2<-paste("\n** ",r," **\n")
    cat(r2)
    print(summary(aov(Licks~Treatment,data=results)))
    print(ggplot(results, aes(results$Treatment, results$Licks)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Licks),max(results$Licks))) + ggtitle(r) + xlab("Treatment") +ylab(ylabel) + guides(fill=FALSE))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        ## Because we are adding licks together, we should transform only after adding!!
        ## So this function should always work on non-transformed data.
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE,TransformLicks=FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        Licks<-results$LicksA+results$LicksB
        if(TransformLicks==TRUE){
          r<-paste("Transformed Total Licks -- Range(min): (",ranges[i,1],",",ranges[i,2],")"
                   ,sep="")
          ylabel<-"Transformed Totoal Licks (A+B)"
          Licks<-Licks^0.25
        }
        else {
          r<-paste("Total Licks -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
          ylabel<-"Total Licks (A+B)"
        }
        results<-data.frame(results,Licks)
        r2<-paste("\n** ",r," **\n")
        cat(r2)
        print(summary(aov(Licks~Treatment,data=results)))
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Licks)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Licks),max(results$Licks))) + ggtitle(r) + xlab("Treatment") +ylab(ylabel) + guides(fill=FALSE))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
 
} 
FeedingEvents.TwoWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingEventsBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      dfm<-GetDFM(monitors[1])
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    Events<-results$EventsA+results$EventsB
    results<-data.frame(results,Events)
    r<-paste("Events-Range: (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Total Events (A+B)") + guides(fill=FALSE))
    cat("** ANOVA results Full Range **\n\n")
    print(summary(aov(Events~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        Events<-results$EventsA+results$EventsB
        results<-data.frame(results,Events)
        r<-paste("Events-Range: (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Total Events (A+B)") + guides(fill=FALSE))
        ppp<-paste("\n** ANOVA results -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        cat(ppp)
        print(summary(aov(Events~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
} 
FeedingMeanDuration.OneWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingMeanDurBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      dfm<-GetDFM(monitors[1])
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    r<-paste("Durations -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$MeanDuration)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Mean Duration (sec)") + guides(fill=FALSE))
    print(summary(aov(MeanDuration~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("Durations -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$MeanDuration)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Mean Duration (sec)") + guides(fill=FALSE))
        print(summary(aov(MeanDuration~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
} 
FeedingMeanTimeBtw.OneWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingMeanBtwBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      dfm<-GetDFM(monitors[1])
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    r<-paste("Time Btw -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$MeanTimeBtw)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$MeanTimeBtw),max(results$MeanTimeBtw))) + ggtitle(r) + xlab("Treatment") +ylab("Time Between") + guides(fill=FALSE))
    print(summary(aov(MeanTimeBtw~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("Time Btw -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$MeanTimeBtw)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$MeanTimeBtw),max(results$MeanTimeBtw))) + ggtitle(r) + xlab("Treatment") +ylab("Time Between") + guides(fill=FALSE))
        print(summary(aov(MeanTimeBtw~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
  
} 
FeedingMeanDuration.TwoWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingMeanDurBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      dfm<-GetDFM(monitors[1])
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    MeanDuration <- ((results$MeanDurationA*results$EventsA)+ (results$MeanDurationB*results$EventsB))/(results$EventsA+results$EventsB)
    results<-data.frame(results,MeanDuration)
    r<-paste("Durations -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$MeanDuration)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Mean Duration (A and B)") + guides(fill=FALSE))
    print(summary(aov(MeanDuration~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        MeanDuration <- ((results$MeanDurationA*results$EventsA)+ (results$MeanDurationB*results$EventsB))/(results$EventsA+results$EventsB)
        results<-data.frame(results,MeanDuration)
        r<-paste("Durations -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$MeanDuration)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Mean Duration (A and B") + guides(fill=FALSE))
        print(summary(aov(MeanDuration~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
  
} 
FeedingMeanTimeBtw.TwoWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingMeanBtwBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      dfm<-GetDFM(monitors[1])
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    MeanTimeBtw <- ((results$MeanTimeBtwA*results$EventsA)+ (results$MeanTimeBtwB*results$EventsB))/(results$EventsA+results$EventsB)
    results<-data.frame(results,MeanTimeBtw)
    r<-paste("Time Btw -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$MeanTimeBtw)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$MeanTimeBtw),max(results$MeanTimeBtw))) + ggtitle(r) + xlab("Treatment") +ylab("Time Between") + guides(fill=FALSE))
    print(summary(aov(MeanTimeBtw~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        MeanTimeBtw <- ((results$MeanTimeBtwA*results$EventsA)+ (results$MeanTimeBtwB*results$EventsB))/(results$EventsA+results$EventsB)
        results<-data.frame(results,MeanTimeBtw)
        r<-paste("Time Btw -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$MeanTimeBtw)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$MeanTimeBtw),max(results$MeanTimeBtw))) + ggtitle(r) + xlab("Treatment") +ylab("Time Between") + guides(fill=FALSE))
        print(summary(aov(MeanTimeBtw~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
  
} 

BinnedFeeding.Summary.OneWell<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE,StartTimeMin=NA,EndTimeMin=NA){
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for single chambers only")
  result<-BinLicks.Well(dfm,1,binsize.min,range,StartTimeMin,EndTimeMin)
  tmp<-BinEvents.Well(dfm,1,binsize.min,range,StartTimeMin,EndTimeMin)
  Events<-tmp$Events
  result<-data.frame(result,Events)
  Well<-factor(rep(1,nrow(result)))
  DFM<-factor(rep(dfm$ID,nrow(result)))
  result<-data.frame(result,DFM,Well)
  
  for(i in 2:12) {
    tmp<-BinLicks.Well(dfm,i,binsize.min,range,StartTimeMin,EndTimeMin)
    tmp2<-BinEvents.Well(dfm,i,binsize.min,range,StartTimeMin,EndTimeMin)
    Events<-tmp2$Events
    tmp<-data.frame(tmp,Events)
    Well<-factor(rep(i,nrow(tmp)))
    DFM<-factor(rep(dfm$ID,nrow(tmp)))
    tmp<-data.frame(tmp,DFM,Well)
    result<-rbind(result,tmp)
  }
  StartMin<-rep(range[1],nrow(result))
  EndMin<-rep(range[2],nrow(result))
  result<-data.frame(result,StartMin,EndMin)
  names(result)<-c("Interval","Min","Licks","Events","DFM","Chamber","StartMin","EndMin")
  ## Note that transformation occurs after the summation.
  if(TransformLicks==TRUE)
    result$Licks<-result$Licks^0.25
  result  
}
BinnedFeeding.Summary.TwoWell<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE,StartTimeMin=NA,EndTimeMin=NA){
  
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  if(dfm$Parameters$PI.Multiplier==1){
    wellA<-dfm$Parameters$Chamber.Sets[1,1]
    wellB<-dfm$Parameters$Chamber.Sets[1,2] 
  }
  else {
    wellB<-dfm$Parameters$Chamber.Sets[1,1]
    wellA<-dfm$Parameters$Chamber.Sets[1,2] 
  }
  
  ltmp<-BinLicks.Well(dfm,wellA,binsize.min,range,StartTimeMin,EndTimeMin)
  ltmp2<-BinLicks.Well(dfm,wellB,binsize.min,range,StartTimeMin,EndTimeMin)
  etmp<-BinEvents.Well(dfm,wellA,binsize.min,range,StartTimeMin,EndTimeMin)
  etmp2<-BinEvents.Well(dfm,wellB,binsize.min,range,StartTimeMin,EndTimeMin)
  Chamber<-factor(rep(1,nrow(ltmp)))
  DFM<-factor(rep(dfm$ID,nrow(ltmp)))
  
  result<-data.frame(ltmp,ltmp2$Licks,etmp$Events,etmp2$Events,DFM,Chamber)
  
  for(i in 2:nrow(dfm$Parameters$Chamber.Sets)) {
    if(dfm$Parameters$PI.Multiplier==1){
      wellA<-dfm$Parameters$Chamber.Sets[i,1]
      wellB<-dfm$Parameters$Chamber.Sets[i,2] 
    }
    else {
      wellB<-dfm$Parameters$Chamber.Sets[i,1]
      wellA<-dfm$Parameters$Chamber.Sets[i,2] 
    }
    
    ltmp<-BinLicks.Well(dfm,wellA,binsize.min,range,StartTimeMin,EndTimeMin)
    ltmp2<-BinLicks.Well(dfm,wellB,binsize.min,range,StartTimeMin,EndTimeMin)
    etmp<-BinEvents.Well(dfm,wellA,binsize.min,range,StartTimeMin,EndTimeMin)
    etmp2<-BinEvents.Well(dfm,wellB,binsize.min,range,StartTimeMin,EndTimeMin)
    Chamber<-factor(rep(i,nrow(ltmp)))
    DFM<-factor(rep(dfm$ID,nrow(ltmp)))
    tmp<-data.frame(ltmp,ltmp2$Licks,etmp$Events,etmp2$Events,DFM,Chamber)
    result<-rbind(result,tmp)
  }
  StartMin<-rep(range[1],nrow(result))
  EndMin<-rep(range[2],nrow(result))
  result<-data.frame(result,StartMin,EndMin)
  names(result)<-c("Interval","Min","LicksA","LicksB","EventsA","EventsB","DFM","Chamber","StartMin","EndMin")
  ## Note that transformation occurs after the summation.
  if(TransformLicks==TRUE){
    result$LicksA<-result$LicksA^0.25
    result$LicksB<-result$LicksB^0.25
  }
  result  
}
BinEvents.Well<-function(dfm,well,binsize.min,range=c(0,0),StartMin=NA,EndMin=NA){
  tmp<-FeedingData.Events(dfm,range)
  cname=paste("W",well,sep="")
  
  tmp<-tmp[,c("Minutes",cname)]
  ## Remember that Event data include duration, but we aren't interested
  ## in that.  Set values >0 to 1.
  tmp[tmp[,cname]>1,cname]<-1
  
  if(is.na(StartMin)){
    m.min<-0
  }
  else {
    m.min<-StartMin
  }
  
  if(is.na(EndMin)){
    m.max<-max(tmp$Minutes)  
  }
  else {
    m.max<-EndMin
  }
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  z<-cut(tmp$Minutes,y,include.lowest=TRUE,dig.lab=8)
  
  r.min<-aggregate(tmp$Minutes~z,FUN=mean)
  r.A<-aggregate(tmp[,cname]~z,FUN=sum)
  
  results<-data.frame(r.min,r.A[,2])
  names(results)<-c("Interval","Min","Events")
  results
}
BinLicks.Well<-function(dfm,well,binsize.min,range=c(0,0),StartMin=NA,EndMin=NA){
  tmp<-FeedingData.Licks(dfm,range)
  cname=paste("W",well,sep="")
  
  tmp<-tmp[,c("Minutes",cname)]
  
  if(is.na(StartMin)){
    m.min<-0
  }
  else {
    m.min<-StartMin
  }
  
  if(is.na(EndMin)){
    m.max<-max(tmp$Minutes)  
  }
  else {
    m.max<-EndMin
  }
  
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  z<-cut(tmp$Minutes,y,include.lowest=TRUE,dig.lab=8)
  
  r.min<-aggregate(tmp$Minutes~z,FUN=mean)
  r.A<-aggregate(tmp[,cname]~z,FUN=sum)
  
  results<-data.frame(r.min,r.A[,2])
  names(results)<-c("Interval","Min","Licks")
  
  #tmp<-aggregate(tmp$ElapsedHours,by=list(Channel=tmp$Channel),max)
  
  
  results
}
## This function accepts a data frame that is assumed to 
## hold results with a column called treatment, that indicates
## treatment
AggregateTreatmentsBinnedData<-function(results){
  trt.summary1<-aggregate(results,by=list(results$Interval,results$Treatment),mean) 
  trt.summary2<-aggregate(results,by=list(results$Interval,results$Treatment),mySEM)
  trt.summary2<-trt.summary2[,-grep("Treatment|DFM|Chamber|Interval",colnames(trt.summary2))]
  trt.summary1<-trt.summary1[,-grep("Treatment|DFM|Chamber|Interval",colnames(trt.summary1))]
  
  
  if("LicksA" %in% names(results)){
    
    tmp<-names(trt.summary1)[4:7]
    tmps<-paste(tmp,"SEM",sep="")
    tmp<-names(trt.summary1)
    tmp<-c(tmp[1:7],tmps,tmp[8:ncol(trt.summary1)])
    tmp[1]<-"Interval"
    tmp[2]<-"Treatment"
    trt.summary<-data.frame(trt.summary1[,1:7],trt.summary2[,4:7],trt.summary1[,8:ncol(trt.summary1)])
    names(trt.summary)<-tmp
    tmp<-1:ncol(trt.summary)
    tmp<-tmp[-2]
    tmp<-c(2,tmp)
    tmp<-trt.summary[,tmp]
  }
  else {
    trt.summary<-data.frame(trt.summary1[,1:5],trt.summary2[,4:5],trt.summary1[,6:ncol(trt.summary1)])
    tmp<-names(trt.summary)
    tmp[1]<-"Interval"
    tmp[2]<-"Treatment"
    tmp[4]<-"Licks"
    tmp[5]<-"Events"
    tmp[6]<-"LicksSEM"
    tmp[7]<-"EventsSEM"
    names(trt.summary)<-tmp
    tmp<-1:ncol(trt.summary)
    tmp<-tmp[-2]
    tmp<-c(2,tmp)
    tmp<-trt.summary[,tmp]
  }
  tmp
}



GetTCWellFromWell<-function(dfm,well){
  if(dfm$Parameters$Chamber.Size==1){
    well<-NA
  }
  else {
    if(well==1 || well==3 || well==5 || well==7 || well==9 || well==11){
      if(dfm$Parameters$PI.Multiplier==1) {
        well<-"WellA"
      }  
      else {
        well<-"WellB"
      }
    }
      
    else {
      if(dfm$Parameters$PI.Multiplier==1) {
        well<-"WellB"
      }  
      else {
        well<-"WellA"
      } 
    }
  }
  well
}
GetChamberFromWell<-function(dfm,well){
  if(dfm$Parameters$Chamber.Size==1){
    chamber<-well
  }
  else {
    if(well==1 || well==2)
      chamber<-1
    else if(well==3 || well==4)
      chamber<-2
    else if(well==5 || well==6)
      chamber<-3
    else if(well==7 || well==8)
      chamber<-4
    else if(well==9 || well==10)
      chamber<-5
    else if(well==11 || well==12)
      chamber<-6
  }
  chamber
}


AggregateTreatments<-function(results){
  trt.summary1<-aggregate(results,by=list(results$Treatment),mean) 
  trt.summary2<-aggregate(results,by=list(results$Treatment),mySEM)
  trt.summary1<-trt.summary1[,-grep("Treatment|DFM|Chamber",colnames(trt.summary1))]
  trt.summary2<-trt.summary2[,-grep("Treatment|DFM|Chamber",colnames(trt.summary2))]
  
  if("LicksA" %in% names(results)){
    names(trt.summary1)[names(trt.summary1) == "Group.1"] <- "Treatment"
    tmp<-names(trt.summary1)[2:19]
    tmp<-paste(tmp,"SEM",sep="")
    trt.summary<-data.frame(trt.summary1[,1:19],trt.summary2[,2:19],trt.summary1[,20:ncol(trt.summary1)])
    names(trt.summary)<-c(names(trt.summary1)[1:19],tmp,names(trt.summary1)[20:ncol(trt.summary1)])
  }
  else {
    names(trt.summary1)[names(trt.summary1) == "Group.1"] <- "Treatment"
    tmp<-names(trt.summary1)[2:9]
    tmp<-paste(tmp,"SEM",sep="")
    trt.summary<-data.frame(trt.summary1[,1:9],trt.summary2[,2:9],trt.summary1[,10:ncol(trt.summary1)])
    names(trt.summary)<-c(names(trt.summary1)[1:9],tmp,names(trt.summary1)[10:ncol(trt.summary1)])
  }
  
  
  #tmp<-c("Treatment","MeanLicks","MeanEvents","MeanMDuration","MeanMedDuration","MeanMTimeBtw","MeanMedTimeBtw","MeanMInt","MeanMedInt",
  #       "SEMLicks","SEMEvents","SEMMDuration","SEMMedDuration","SEMMTimeBtw","SEMMedTimeBtw","SEMMInt","SEMMedInt",names(trt.summary)[18:ncol(trt.summary)])
  
  #names(trt.summary)<-tmp
  trt.summary
}
PlotBins.Licks.DFM.OneWell<-function(dfm,binsize.min=30,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for single chambers only")
  binnedData<-BinnedFeeding.Summary.DFM(dfm,binsize.min,range,TransformLicks)
  ylabel<-"Licks"
  xlabel<-"Minutes"
  ttl<-paste("DFM:",dfm$ID)
  if(TransformLicks==TRUE) {
    ylabel<-"Transformed Licks"
    ttl<-paste("DFM:",dfm$ID," (Transformed)")
  }
  gp<-ggplot(binnedData,aes(x=Min,y=Licks,fill=Chamber)) + geom_bar(stat="identity") + facet_grid(Chamber ~ .) + ggtitle(paste("DFM:",dfm$ID)) +
    theme(legend.position = "none") + ylab(ylabel) + xlab(xlabel)
  show(gp)
}
PlotBins.Licks.DFM.TwoWell<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-well chambers only")
  binnedData<-BinnedFeeding.Summary.DFM(dfm,binsize.min,range,TransformLicks)
  ylabel<-"Licks"
  xlabel<-"Minutes"
  ttl<-paste("DFM:",dfm$ID)
  if(TransformLicks==TRUE) {
    ylabel<-"Transformed Licks"
    ttl<-paste("DFM:",dfm$ID," (Transformed)")
  }
  tmp2<-melt(binnedData,id.vars=c("Min","Chamber"),measure.vars=c("LicksA","LicksB"))
  names(tmp2)[3]<-"Well"
  gp<-ggplot(tmp2,aes(x=Min,y=value,fill=Well)) + geom_bar(stat="identity") + facet_grid(Chamber ~ .) + ggtitle(paste("DFM:",dfm$ID)) +
    ylab(ylabel) + xlab(xlabel)
  show(gp)
}
## These functions will output the intervals
GetIntervalData.Well<-function(dfm,well, range=c(0,0)){
  nameA<-paste("W",well,sep="")
  parameter.vector<-GetDFMParameterVector(dfm)
  pnames<-Get.Parameter.Names(dfm$Parameters)
  
  theData<-dfm$Intervals[[nameA]]
  
  Well<-rep(well,nrow(theData))
  chamber<-rep(GetChamberFromWell(dfm,well),nrow(theData))
  TCWell<-rep(GetTCWellFromWell(dfm,well),nrow(theData))
  DFM<-rep(dfm$ID,nrow(theData))
  
  tmpA<-data.frame(DFM,chamber,TCWell,Well,theData)
  tmp2<-matrix(rep(parameter.vector,nrow(tmpA)),ncol=length(parameter.vector),byrow=TRUE)
  tmp3<-data.frame(tmpA,tmp2)
  names(tmp3)<-c("DFM","Chamber","TCWell","Well","Minutes","Sample","IntervalSec",pnames)
  
  if(dfm$Parameters$Chamber.Size==1)
    tmp3<-tmp3[,!names(tmp3) %in% "TCWell"]
  
  tmp3
}
BinnedLicksPlot.TwoWell.Trt<-function(monitors,parameters,binsize.min=20,expDesign,range=c(0,0),SaveToFile=FALSE,TransformLicks=TRUE){

  if(TransformLicks==TRUE){
    ylabel<-"Transformed Licks"
  }
  else {
    ylabel<-"Licks"
  }
  
  data<-BinnedFeeding.Summary.Monitors(monitors,parameters,binsize.min,expDesign,range,SaveToFile,TransformLicks)  
  tmpA<-data.frame(data$Stats[,c(1,3,4,8)],rep("WellA",nrow(data$Stats)))
  names(tmpA)<-c("Treatment","Min","Licks","LicksSEM","Well")
  tmpB<-data.frame(data$Stats[,c(1,3,5,9)],rep("WellB",nrow(data$Stats)))
  names(tmpB)<-c("Treatment","Min","Licks","LicksSEM","Well")
  
  newData<-rbind(tmpA,tmpB)
  
  pd <- position_dodge(5) # move them .05 to the left and right
  gp<-ggplot(newData,aes(x=Min,y=Licks,color=Treatment,group=Treatment)) + 
    geom_errorbar(aes(ymin=Licks-LicksSEM, ymax=Licks+LicksSEM,color=Treatment), width=.1, position=pd) +
    geom_line(position=pd,size=1) + facet_wrap(~Well)+
    geom_point(position=pd, size=4, shape=21, fill="white") +xlab("Minutes") + ylab(ylabel)
  show(gp)
  
  if(SaveToFile==TRUE){
    filename<-paste("BinnedLicksPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    ggsave(filename,gp)
  }
  
  tmp2<-data$Results
  l<-lapply(split(tmp2, tmp2$Interval), aov, formula=LicksA ~ Treatment)
  cat("\n\n\n** Interval specific ANOVA results for Well A **\n\n")
  print(lapply(l,summary))
  
  l<-lapply(split(tmp2, tmp2$Interval), aov, formula=LicksB ~ Treatment)
  cat("\n\n\n** Interval specific ANOVA results for Well B **\n\n")
  print(lapply(l,summary))
}
BinnedLicksPlot.OneWell.Trt<-function(monitors,parameters,binsize.min=20,expDesign,range=c(0,0),SaveToFile=FALSE,TransformLicks=TRUE){
  if(parameters$Chamber.Size!=1)
    stop("This function is for one chamber DFM only")
  
  if(TransformLicks==TRUE){
    ylabel<-"Transformed Licks"
  }
  else {
    ylabel<-"Licks"
  }
  
  data<-BinnedFeeding.Summary.Monitors(monitors,parameters,binsize.min,expDesign,range,SaveToFile,TransformLicks)  
  tmp<-data$Stats
  
  pd <- position_dodge(5) # move them .05 to the left and right
  gp<-ggplot(tmp,aes(x=Min,y=Licks,color=Treatment,group=Treatment)) + 
    geom_errorbar(aes(ymin=Licks-LicksSEM, ymax=Licks+LicksSEM,color=Treatment), width=.1, position=pd) +
    geom_line(position=pd,size=1) +
    geom_point(position=pd, size=4, shape=21, fill="white") +xlab("Minutes") + ylab(ylabel)
  show(gp)
  
  
  if(SaveToFile==TRUE){
    filename<-paste("BinnedLicksPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    ggsave(filename,gp)
  }
  
  
  tmp2<-data$Results
  l<-lapply(split(tmp2, tmp2$Interval), aov, formula=Licks ~ Treatment)
  cat("** Interval specific ANOVA results **\n\n")
  lapply(l,summary)
}

BinnedEventsPlot.TwoWell.Trt<-function(monitors,parameters,binsize.min=20,expDesign,range=c(0,0),SaveToFile=FALSE){
  
  data<-BinnedFeeding.Summary.Monitors(monitors,parameters,binsize.min,expDesign,range,SaveToFile)  
  tmpA<-data.frame(data$Stats[,c(1,3,6,10)],rep("WellA",nrow(data$Stats)))
  names(tmpA)<-c("Treatment","Min","Events","EventsSEM","Well")
  tmpB<-data.frame(data$Stats[,c(1,3,7,11)],rep("WellB",nrow(data$Stats)))
  names(tmpB)<-c("Treatment","Min","Events","EventsSEM","Well")
  
  newData<-rbind(tmpA,tmpB)
  
  pd <- position_dodge(5) # move them .05 to the left and right
  gp<-ggplot(newData,aes(x=Min,y=Events,color=Treatment,group=Treatment)) + 
    geom_errorbar(aes(ymin=Events-EventsSEM, ymax=Events+EventsSEM,color=Treatment), width=.1, position=pd) +
    geom_line(position=pd,size=1) + facet_wrap(~Well)+
    geom_point(position=pd, size=4, shape=21, fill="white") +xlab("Minutes") + ylab("Events")
  show(gp)
  
  if(SaveToFile==TRUE){
    filename<-paste("BinnedEventsPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    ggsave(filename,gp)
  }
  
  tmp2<-data$Results
  l<-lapply(split(tmp2, tmp2$Interval), aov, formula=LicksA ~ Treatment)
  cat("\n\n\n** Interval specific ANOVA results for Well A **\n\n")
  print(lapply(l,summary))
  
  l<-lapply(split(tmp2, tmp2$Interval), aov, formula=LicksB ~ Treatment)
  cat("\n\n\n** Interval specific ANOVA results for Well B **\n\n")
  print(lapply(l,summary))
}
BinnedEventsPlot.OneWell.Trt<-function(monitors,parameters,binsize.min=20,expDesign,range=c(0,0),SaveToFile=FALSE){
 
  data<-BinnedFeeding.Summary.Monitors(monitors,parameters,binsize.min,expDesign,range,SaveToFile)  
  tmp<-data$Stats
  
  pd <- position_dodge(5) # move them .05 to the left and right
  gp<-ggplot(tmp,aes(x=Min,y=Events,color=Treatment,group=Treatment)) + 
    geom_errorbar(aes(ymin=Events-EventsSEM, ymax=Events+EventsSEM,color=Treatment), width=.1, position=pd) +
    geom_line(position=pd,size=1) +
    geom_point(position=pd, size=4, shape=21, fill="white") +xlab("Minutes") + ylab("Events")
  show(gp)
  
  
  if(SaveToFile==TRUE){
    filename<-paste("BinnedEventsPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    ggsave(filename,gp)
  }
  
  
  tmp2<-data$Results
  l<-lapply(split(tmp2, tmp2$Interval), aov, formula=Licks ~ Treatment)
  cat("** Interval specific ANOVA results **\n\n")
  lapply(l,summary)
}


## Private Utilities
mySEM<-function(x){
  if(is.factor(x))
    tmp<-unique(as.character(x))
  
  else
    tmp<-sqrt(var(x)/length(x))
  tmp
}
GetTreatmentForChamber<-function(dfmNum,chamberNum,expdesign){
  tmp<-subset(expdesign,DFM==dfmNum)
  tmp<-subset(tmp,Chamber==chamberNum)
  if(nrow(tmp)!=1)
    return("None")
  else
    return(as.character(tmp$Treatment))
}
gt<-function(df,expDesign){
  return("hi")
}
GetTreatmentForRow<-function(df,expdesign){
  tmp<-subset(expdesign,DFM==df["DFM"] & Chamber==df["Chamber"])
  if(nrow(tmp)!=1)
    return("None")
  else
    return(as.character(tmp$Treatment))
}
## Need to fix treatment assignments in single and choice experiments!!
AppendTreatmentonResultsFrame<-function(results,expdesign){
  isChamberThere<-"Chamber" %in% names(results)
  if(isChamberThere==FALSE)
    stop("Need a chamber to assign treatment.")
  Treatment<-rep(NA,nrow(results))
  for(i in 1:nrow(results)){
    Treatment[i]<-GetTreatmentForChamber(results$DFM[i],results$Chamber[i],expdesign)
  }
  n<-names(results)
  n<-c("Treatment",n)
  results<-cbind(Treatment,results)
  names(results)<-n
  results
}
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


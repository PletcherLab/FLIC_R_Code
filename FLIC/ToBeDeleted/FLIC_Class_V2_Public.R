
## Public Class Methods for FLIC Class V2
source("FLIC_Class_V2_Private.R")


## Use this function to change a parameter that has already been assigned to 
## a chamber.  When a parameter is changed, all relevant stats and calculations
## are updated to reflect the new parameter value.
ChangeParameter<-function(chamber,Baseline.Window.Minutes=NA,Feeding.Threshold.Value=NA, Feeding.Interval.Minimum=NA, Tasting.Threshold.Interval=NA,Use.Adaptive.Threshold=NA, 
                          Adaptive.Threshold.Minimum=NA, Adaptive.Threshold.Window.Minutes=NA, Adaptive.Threshold.Selection.Quant=NA,Feeding.Minevents=NA,
                          Samples.Per.Sec=NA) {
  p<-chamber$Parameters
  baseline.flag<-FALSE
  threshold.flag<-FALSE
  adaptive.baseline.flag<-FALSE
  eventpi.flag<-FALSE
  tmp.O<-options()
  options(warn=-1)
  ## Change only those that are listed
  if(!is.na(Baseline.Window.Minutes)) {
    p$Baseline.Window.Minutes=Baseline.Window.Minutes
    baseline.flag<-TRUE
  }
  if(!is.na(Feeding.Threshold.Value)) {
    p$Feeding.Threshold.Value=Feeding.Threshold.Value
    threshold.flag<-TRUE
  }
  if(!is.na(Feeding.Interval.Minimum)) {
    p$Feeding.Interval.Minimum=Feeding.Interval.Minimum
    threshold.flag<-TRUE
  }
  if(!is.na(Tasting.Threshold.Interval)) {
    p$Tasting.Threshold.Interval.Low=Tasting.Threshold.Interval[1]
    p$Tasting.Threshold.Interval.High=Tasting.Threshold.Interval[2]
    threshold.flag<-TRUE
  }
  if(!is.na(Adaptive.Threshold.Minimum)){
    p$Adaptive.Threshold.Minimum=Adaptive.Threshold.Minimum
    threshold.flag<-TRUE
  }
  if(!is.na(Adaptive.Threshold.Window.Minutes)){
    p$Adaptive.Threshold.Window.Minutes=Adaptive.Threshold.Window.Minutes
    adaptive.baseline.flag<-TRUE
  }
  if(!is.na(Adaptive.Threshold.Selection.Quant)){
    p$Adaptive.Threshold.Selection.Quant=Adaptive.Threshold.Selection.Quant
    adaptive.baseline.flag<-TRUE
  }
  if(!is.na(Use.Adaptive.Threshold)){
    p$Use.Adaptive.Threshold=Use.Adaptive.Threshold
    adaptive.baseline.flag<-TRUE
  }
  if(!is.na(Feeding.Minevents)){
    p$Feeding.Minevents=Feeding.Minevents
    eventpi.flag<-TRUE
  }
  if(!is.na(Samples.Per.Sec)){
    p$Samples.Per.Second=Samples.Per.Second
    adaptive.baseline.flag<-TRUE
  }
  
  chamber$Parameters<-p
  
  ## Now update the stats needed
  if(baseline.flag==TRUE) {
    chamber<-CalculateBaseline(chamber)
  }
  else if(adaptive.baseline.flag==TRUE){
    chamber<-SetThreshold(chamber)
  }
  else if(threshold.flag==TRUE) {
    chamber<-SetThreshold(chamber,getStandard=FALSE)
  }
  else if(eventpi.flag==TRUE) {
    chamber<-Set.Feeding.Data(chamber)
    chamber<-Set.Tasting.Data(chamber)
    chamber<-Set.Durations.And.Intervals(chamber)
  }
  options(tmp.O)
  UpdateHiddenChamberObject(chamber)
  chamber  
}
ChangeParameterObject<-function(chamber,newP) {
  p<-chamber$Parameters
  baseline.flag<-FALSE
  threshold.flag<-FALSE
  adaptive.baseline.flag<-FALSE
  eventpi.flag<-FALSE
  tmp.O<-options()
  options(warn=-1)
  chamber$Parameters<-newP
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
  if(p$Samples.Per.Second!=newP$Samples.Per.Second){
    adaptive.baseline.flag<-TRUE
  }
    
  
  ## Now update the stats needed
  if(baseline.flag==TRUE) {
    chamber<-CalculateBaseline(chamber)
  }
  else if(adaptive.baseline.flag==TRUE){
    chamber<-SetThreshold(chamber)
  }
  else if(threshold.flag==TRUE) {
    chamber<-SetThreshold(chamber,getStandard=FALSE)
  }
  else if(eventpi.flag==TRUE) {
    chamber<-Set.Feeding.Data(chamber)
    chamber<-Set.Tasting.Data(chamber)
    chamber<-Set.Durations.And.Intervals(chamber)
  }
  options(tmp.O)
  UpdateHiddenChamberObject(chamber)
  chamber  
}




## These are accessors, which are used to retrieve certain aspects of
## the data from the chamber in question. I think most are self explanatory.
## These, along with the chamber-specific helper functions should be very 
## useful in exploring details of individual chambers.

GetChamberParameterVector<-function(chamber){
  GetParameterVector(chamber$Parameters)
}
LastSampleData<-function(chamber){
  tmp<-BaselinedData(chamber)
  nr<-nrow(tmp)
  tmp[nr,]
}
FirstSampleData<-function(chamber){
  tmp<-BaselinedData(chamber)  
  tmp[1,]
}

## Feeding-related function
Feeding.MinThresholds<-function(chamber,range=c(0,0)) { 
  tmp<-Thresholds(chamber,range)
  data.frame(tmp$FeedingMinA,tmp$FeedingMinB)
}


SecondsToFirstFeeding<-function(chamber,starttime.min=0){
  result<-c(NA,NA)
  tmp<-chamber$FeedingDurations$ADurations
  if(is.data.frame(tmp)){
    if(nrow(tmp)>=1){    
      for(i in 1:nrow(tmp)){
        if(tmp$Minutes[i]>starttime.min){
          result[1]<-(tmp$Minutes[i]-starttime.min)*60        
          break
        }        
      }
    }    
  }
  
  tmp<-chamber$FeedingDurations$BDurations
  if(is.data.frame(tmp)){
    if(nrow(tmp)>=1){
      if(nrow(tmp)>=1){      
        for(i in 1:nrow(tmp)){
          if(tmp$Minutes[i]>starttime.min) {
            result[2]<-(tmp$Minutes[i]-starttime.min)*60       
            break;
          }
        }
      }   
    }      
  }
  names(result)<-c("PlateA","PlateB")
  result
}
SecondsBtwFeedings<-function(chamber,high.event){
  result<-c(NA,NA)
  tmp<-chamber$FeedingIntervals$AIntervals
  if(nrow(tmp)>=high.event)
    result[1]<-tmp$IntervalSec[high.event]
  
  tmp<-chamber$FeedingIntervals$BIntervals
  if(nrow(tmp)>=high.event)
    result[2]<-tmp$IntervalSec[high.event]
  
  names(result)<-c("PlateA","PlateB")
  result
}

Feeding.TotalIntensity<-function(chamber,range=c(0,0)){
  pi<-FeedingData(chamber,range)
  bd<-BaselinedData(chamber,range)
  tmp.A<-bd$AnalogA[pi$FeedingLicksA]
  tmp.B<-bd$AnalogB[pi$FeedingLicksB]
  c(sum(tmp.A),sum(tmp.B))
}

## Tasting-related functions
Tasting.FinalPI<-function(chamber,range=c(0,0)) {
  if(sum(range)!=0) {
    tmp<-TastingData(chamber,range=range)
    n<-sum(tmp$Tasting.PI)
    d<-sum(abs(tmp$Tasting.PI))    
  }
  else {
    n<-sum(chamber$TastingData$Tasting.PI)
    d<-sum(abs(chamber$TastingData$Tasting.PI))    
  }
  if(d==0)
    result<-0
  else
    result<-n/d  
  result
}
Tasting.Summary<-function(chamber,range=c(0,0)){
  PI<-Tasting.FinalPI(chamber,range)
  Licks<-Tasting.TotalLicks(chamber,range)  
  result<-data.frame(matrix(c(PI,Licks,range[1],range[2]),nrow=1))
  names(result)<-c("PI","LicksA","LicksB","StartMin","EndMin")
  result 
}
Tasting.CumulativePIData<-function(chamber,range=c(0,0)){
  d<-TastingData(chamber,range)
  
  n<-cumsum(d$Tasting.PI)
  den<-cumsum(abs(d$Tasting.PI))
  y<-n/den
  y[den==0]<-0
  
  Tasting.PI<-y
  CumLicksA<-cumsum(d$TastingLicksA)
  CumLicksB<-cumsum(d$TastingLicksB)
  
  result<-data.frame(d$Minutes,d$Sample,Tasting.PI,CumLicksA,CumLicksB)
  names(result)<-c("Minutes","Sample","Tasting.PI","CumLicksA","CumLicksB")
  result
}

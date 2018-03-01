
## First, the short parameters class
## It is instantiated with a set of initial values
ParametersClass=function(){
  Baseline.Window.Minutes=3
  Signal.Threshold = 50
  Feeding.Threshold.Value=100
  Feeding.Interval.Minimum=20
  Tasting.Threshold.Interval=c(10,80)
  Use.Adaptive.Threshold=FALSE
  Adaptive.Threshold.Minimum=8
  Adaptive.Threshold.Window.Minutes=2
  Adaptive.Threshold.Selection.Quant=0.9
  Feeding.Minevents=2
  Samples.Per.Second=5
  Chamber.Sets=matrix(1:12,ncol=2,byrow=TRUE)
  Chamber.Size=2
  list(Baseline.Window.Minutes=Baseline.Window.Minutes,Signal.Threshold=Signal.Threshold,Feeding.Threshold.Value=Feeding.Threshold.Value,
       Feeding.Interval.Minimum=Feeding.Interval.Minimum,Tasting.Threshold.Interval.Low=Tasting.Threshold.Interval[1],
       Tasting.Threshold.Interval.High=Tasting.Threshold.Interval[2],
       Use.Adaptive.Threshold=Use.Adaptive.Threshold,Adaptive.Threshold.Minimum=Adaptive.Threshold.Minimum,       
       Adaptive.Threshold.Window.Minutes=Adaptive.Threshold.Window.Minutes,Adaptive.Threshold.Selection.Quant=Adaptive.Threshold.Selection.Quant,
       Feeding.Minevents=Feeding.Minevents,Samples.Per.Second=Samples.Per.Second,Chamber.Size=Chamber.Size,Chamber.Sets=Chamber.Sets)
}

ParametersClass.SingleWell=function(){
  Baseline.Window.Minutes=3
  Signal.Threshold = 50
  Feeding.Threshold.Value=150
  Feeding.Interval.Minimum=50  
  Tasting.Threshold.Interval=c(10,50)
  Use.Adaptive.Threshold=FALSE
  Adaptive.Threshold.Minimum=8
  Adaptive.Threshold.Window.Minutes=2
  Adaptive.Threshold.Selection.Quant=0.9
  Feeding.Minevents=5
  Samples.Per.Second=5
  Chamber.Sets=matrix(1:12,ncol=1,byrow=TRUE)
  Chamber.Size=1
  list(Baseline.Window.Minutes=Baseline.Window.Minutes,Signal.Threshold=Signal.Threshold,Feeding.Threshold.Value=Feeding.Threshold.Value,
       Feeding.Interval.Minimum=Feeding.Interval.Minimum,Tasting.Threshold.Interval.Low=Tasting.Threshold.Interval[1],
       Tasting.Threshold.Interval.High=Tasting.Threshold.Interval[2],
       Use.Adaptive.Threshold=Use.Adaptive.Threshold,Adaptive.Threshold.Minimum=Adaptive.Threshold.Minimum,       
       Adaptive.Threshold.Window.Minutes=Adaptive.Threshold.Window.Minutes,Adaptive.Threshold.Selection.Quant=Adaptive.Threshold.Selection.Quant,
       Feeding.Minevents=Feeding.Minevents,Samples.Per.Second=Samples.Per.Second,Chamber.Size=Chamber.Size,Chamber.Sets=Chamber.Sets)
}

ParametersClass.TwoWell=function(){
  Baseline.Window.Minutes=3
  Signal.Threshold = 50
  Feeding.Threshold.Value=150
  Feeding.Interval.Minimum=50
  Tasting.Threshold.Interval=c(10,50)
  Use.Adaptive.Threshold=FALSE
  Adaptive.Threshold.Minimum=8
  Adaptive.Threshold.Window.Minutes=2
  Adaptive.Threshold.Selection.Quant=0.9
  Feeding.Minevents=5
  Samples.Per.Second=5
  Chamber.Sets=matrix(1:12,ncol=2,byrow=TRUE)
  Chamber.Size=2
  list(Baseline.Window.Minutes=Baseline.Window.Minutes,Signal.Threshold=Signal.Threshold,Feeding.Threshold.Value=Feeding.Threshold.Value,
       Feeding.Interval.Minimum=Feeding.Interval.Minimum,Tasting.Threshold.Interval.Low=Tasting.Threshold.Interval[1],
       Tasting.Threshold.Interval.High=Tasting.Threshold.Interval[2],
       Use.Adaptive.Threshold=Use.Adaptive.Threshold,Adaptive.Threshold.Minimum=Adaptive.Threshold.Minimum,       
       Adaptive.Threshold.Window.Minutes=Adaptive.Threshold.Window.Minutes,Adaptive.Threshold.Selection.Quant=Adaptive.Threshold.Selection.Quant,
       Feeding.Minevents=Feeding.Minevents,Samples.Per.Second=Samples.Per.Second,Chamber.Size=Chamber.Size,Chamber.Sets=Chamber.Sets)
}

## change the initial values using this function
SetParameter<-function(p,Baseline.Window.Minutes=NA,Feeding.Threshold.Value=NA, Feeding.Interval.Minimum=NA, Tasting.Threshold.Interval=NA,Use.Adaptive.Threshold=NA, 
                       Adaptive.Threshold.Minimum=NA, Adaptive.Threshold.Window.Minutes=NA, Adaptive.Threshold.Selection.Quant=NA,Feeding.Minevents=NA,
                       Samples.Per.Sec=NA, Chamber.Size=NA, Signal.Threshold=NA){
  tmp.O<-options()
  options(warn=-1)
  ## Change only those that are listed
  if(!is.na(Baseline.Window.Minutes)) {
    p$Baseline.Window.Minutes=Baseline.Window.Minutes  
  }
  if(!is.na(Feeding.Threshold.Value)) {
    p$Feeding.Threshold.Value=Feeding.Threshold.Value
  }
  if(!is.na(Feeding.Interval.Minimum)) {
    p$Feeding.Interval.Minimum=Feeding.Interval.Minimum
  }
  if(!is.na(Tasting.Threshold.Interval)) {
    p$Tasting.Threshold.Interval.Low=Tasting.Threshold.Interval[1]
    p$Tasting.Threshold.Interval.High=Tasting.Threshold.Interval[2]
  }
  if(!is.na(Adaptive.Threshold.Minimum)){
    p$Adaptive.Threshold.Minimum=Adaptive.Threshold.Minimum
  }
  if(!is.na(Adaptive.Threshold.Window.Minutes)){
    p$Adaptive.Threshold.Window.Minutes=Adaptive.Threshold.Window.Minutes
  }
  if(!is.na(Adaptive.Threshold.Selection.Quant)){
    p$Adaptive.Threshold.Selection.Quant=Adaptive.Threshold.Selection.Quant
  }
  if(!is.na(Use.Adaptive.Threshold)){
    p$Use.Adaptive.Threshold=Use.Adaptive.Threshold
  }
  if(!is.na(Feeding.Minevents)){
    p$Feeding.Minevents=Feeding.Minevents
  }
  if(!is.na(Samples.Per.Sec)){
    p$Samples.Per.Second=Samples.Per.Second
  }
  if(!is.na(Chamber.Size)){
    p$Chamber.Size=Chamber.Size
  }
  if(!is.na(Signal.Threshold)){
    p$Signal.Threshold=Signal.Threshold
  }
  options(tmp.O)
  p
  
}

Get.Parameter.Names<-function(parameters){
  chambernames<-paste("Ch",1:length(parameters$Chamber.Sets),sep="")
  result<-c("BaselineWindowMin","SignalThreshold","FeedingThreshold","FeedingMinimum","TastingLow","TastingHigh",
            "UseAdaptiveThresh","AdapThreshMin","AdapThreshWinMin","AdapThreshSelQuant",
            "FeedingMinEvents","SamplesSec","ChamberSize",chambernames)
  result
}


## This is used internally to return the group of parameters in vector form.
GetParameterVector<-function(parameters){  
  unlist(parameters)   
}
AreParametersEqual<-function(p1,p2){
  result<-TRUE
  if(p1$Baseline.Window.Minutes!=p2$Baseline.Window.Minutes) {
    result<-FALSE
  }
  
  if(p1$Feeding.Threshold.Value!=p2$Feeding.Threshold.Value) {
    result<-FALSE
  }
  if(p1$Feeding.Interval.Minimum!=p2$Feeding.Interval.Minimum) {
    result<-FALSE
  }
  if(p1$Tasting.Threshold.Interval.Low!=p2$Tasting.Threshold.Interval.Low) {
    result<-FALSE
  }
  if(p1$Tasting.Threshold.Interval.High!=p2$Tasting.Threshold.Interval.High) {
    result<-FALSE
  }
  if(p1$Use.Adaptive.Threshold!=p2$Use.Adaptive.Threshold) {
    result<-FALSE
  }
  if(p1$Adaptive.Threshold.Minimum!=p2$Adaptive.Threshold.Minimum){
    result<-FALSE
  }
  if(p1$Adaptive.Threshold.Window.Minutes!=p2$Adaptive.Threshold.Window.Minutes){
    result<-FALSE
  }
  if(p1$Adaptive.Threshold.Selection.Quant!=p2$Adaptive.Threshold.Selection.Quant){
    result<-FALSE
  }
  if(p1$Feeding.Minevents!=p2$Feeding.Minevents){
    result<-FALSE
  }
  if(p1$Chamber.Size!=p2$Chamber.Size) {
    result<-FALSE
  }
  if(p1$Signal.Threshold!=p2$Signal.Threshold) {
    result<-FALSE
  }
  result
}

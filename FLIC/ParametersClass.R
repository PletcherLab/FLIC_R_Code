
## First, the short parameters class
## It is instantiated with a set of initial values
ParametersClass=function(){
  Baseline.Window.Minutes=3
  Feeding.Threshold=20
  Feeding.Minimum=10
  Tasting.Interval=c(10,20)
  Feeding.Minevents=1
  Tasting.Minevents=1
  Samples.Per.Second=5
  Feeding.Event.Link.Gap=5
  Chamber.Sets=matrix(1:12,ncol=2,byrow=TRUE)
  Chamber.Size=2
  PI.Multiplier=1
  list(Baseline.Window.Minutes=Baseline.Window.Minutes,Feeding.Threshold=Feeding.Threshold,
       Feeding.Minimum=Feeding.Minimum,Tasting.Minimum=Tasting.Interval[1],
       Tasting.Maximum=Tasting.Interval[2],
       Feeding.Minevents=Feeding.Minevents,Tasting.Minevents=Tasting.Minevents,Samples.Per.Second=Samples.Per.Second,Chamber.Size=Chamber.Size,
       Chamber.Sets=Chamber.Sets,Feeding.Event.Link.Gap=Feeding.Event.Link.Gap,PI.Multiplier=PI.Multiplier)
}
ParametersClass.SingleWell=function(){
  Baseline.Window.Minutes=3
  Feeding.Threshold=20
  Feeding.Minimum=10  
  Tasting.Interval=c(10,20)
  Feeding.Minevents=1
  Tasting.Minevents=1
  Samples.Per.Second=5
  Chamber.Sets=matrix(1:12,ncol=1,byrow=TRUE)
  Chamber.Size=1
  PI.Multiplier=0
  Feeding.Event.Link.Gap=5
  list(Baseline.Window.Minutes=Baseline.Window.Minutes,Feeding.Threshold=Feeding.Threshold,
       Feeding.Minimum=Feeding.Minimum,Tasting.Minimum=Tasting.Interval[1],
       Tasting.Maximum=Tasting.Interval[2],
       Feeding.Minevents=Feeding.Minevents,Tasting.Minevents=Tasting.Minevents,Samples.Per.Second=Samples.Per.Second,Chamber.Size=Chamber.Size,
       Chamber.Sets=Chamber.Sets,Feeding.Event.Link.Gap=Feeding.Event.Link.Gap,PI.Multiplier=PI.Multiplier)
}
ParametersClass.TwoWell=function(){
  Baseline.Window.Minutes=3
  Feeding.Threshold=20
  Feeding.Minimum=10
  Tasting.Interval=c(10,20)
  Feeding.Minevents=1
  Tasting.Minevents=1
  Samples.Per.Second=5
  Chamber.Sets=matrix(1:12,ncol=2,byrow=TRUE)
  Chamber.Size=2
  PI.Multiplier=1
  Feeding.Event.Link.Gap=5
  list(Baseline.Window.Minutes=Baseline.Window.Minutes,Feeding.Threshold=Feeding.Threshold,
       Feeding.Minimum=Feeding.Minimum,Tasting.Minimum=Tasting.Interval[1],
       Tasting.Maximum=Tasting.Interval[2],
       Feeding.Minevents=Feeding.Minevents,Tasting.Minevents=Tasting.Minevents,Samples.Per.Second=Samples.Per.Second,Chamber.Size=Chamber.Size,
       Chamber.Sets=Chamber.Sets,Feeding.Event.Link.Gap=Feeding.Event.Link.Gap,PI.Multiplier=PI.Multiplier)
}

## change the initial values using this function
SetParameter<-function(p,Baseline.Window.Minutes=NA,Feeding.Threshold=NA, Feeding.Minimum=NA, Tasting.Interval=NA,
                       Feeding.Minevents=NA,Tasting.Minevents=NA,
                       Samples.Per.Sec=NA, Chamber.Size=NA, Feeding.Event.Link.Gap=NA,PI.Multiplier=NA){
  tmp.O<-options()
  options(warn=-1)
  ## Change only those that are listed
  if(!is.na(Baseline.Window.Minutes)) {
    p$Baseline.Window.Minutes=Baseline.Window.Minutes  
  }
  if(!is.na(Feeding.Threshold)) {
    p$Feeding.Threshold=Feeding.Threshold
  }
  if(!is.na(Feeding.Minimum)) {
    p$Feeding.Minimum=Feeding.Minimum
  }
  if(!is.na(Tasting.Interval)) {
    p$Tasting.Minimum=Tasting.Interval[1]
    p$Tasting.Maximum=Tasting.Interval[2]
  }
  if(!is.na(Feeding.Minevents)){
    p$Feeding.Minevents=Feeding.Minevents
  }
  if(!is.na(Tasting.Minevents)){
    p$Tasting.Minevents=Tasting.Minevents
  }
  if(!is.na(Samples.Per.Sec)){
    p$Samples.Per.Second=Samples.Per.Sec
  }
  if(!is.na(Chamber.Size)){
    p$Chamber.Size=Chamber.Size
  }
  if(!is.na(PI.Multiplier)){
    p$PI.Multiplier=PI.Multiplier
  }
  if(!is.na(Feeding.Event.Link.Gap)){
    p$Feeding.Event.Link.Gap=Feeding.Event.Link.Gap
  }
  options(tmp.O)
  p
  
}

Get.Parameter.Names<-function(parameters){
  chambernames<-paste("Ch",1:length(parameters$Chamber.Sets),sep="")
  result<-c("BaselineWindowMin","FeedingThreshold","FeedingMinimum","TastingLow","TastingHigh",
            "FeedingMinEvents","TastingMinEvents","SamplesSec","ChamberSize",chambernames,"Link.Gap","PI.Multiplier")
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
  
  if(p1$Feeding.Threshold!=p2$Feeding.Threshold) {
    result<-FALSE
  }
  if(p1$Feeding.Minimum!=p2$Feeding.Minimum) {
    result<-FALSE
  }
  if(p1$Tasting.Minimum!=p2$Tasting.Minimum) {
    result<-FALSE
  }
  if(p1$Tasting.Maximum!=p2$Tasting.Maximum) {
    result<-FALSE
  }
  if(p1$Feeding.Minevents!=p2$Feeding.Minevents){
    result<-FALSE
  }
  if(p1$Tasting.Minevents!=p2$Tasting.Minevents){
    result<-FALSE
  }
  if(p1$Chamber.Size!=p2$Chamber.Size) {
    result<-FALSE
  }
  if(p1$Feeding.Event.Link.Gap!=p2$Feeding.Event.Link.Gap) {
    result<-FALSE
  }
  if(p1$PI.Multiplier!=p2$PI.Multiplier) {
    result<-FALSE
  }
  result
}

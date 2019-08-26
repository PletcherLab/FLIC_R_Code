source("PrivateFunctions.R")
source("ParametersClass.R")

## These are functions that are generally available to the user ##
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
DFMClass.LinkFiles<-function(id,parameters,range=c(0,0)) {
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
    
    tmp<-paste("DFM_",id,"_.*[.]csv",sep="")
    files<-list.files(pattern=tmp)
    
    dfm<-read.csv(files[1],header=TRUE)  
    print(paste("Reading DFM File:",files[1]))
    for(i in 2:length(files)){ 
      print(paste("Reading DFM File:",files[i]))
      tmp<-read.csv(files[i],header=TRUE)  
      dfm<-rbind(dfm,tmp)
    }
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
LastSampleData<-function(dfm){
  tmp<-BaselinedData(dfm)
  nr<-nrow(tmp)
  tmp[nr,]
}
FirstSampleData<-function(dfm){
  tmp<-BaselinedData(dfm)  
  tmp[1,]
}
BaselineData<-function(dfm,range=c(0,0)){
  tmp<-dfm$BaselineData
  if(sum(range)!=0) {
    tmp<- tmp[(tmp$Minutes>range[1]) & (tmp$Minutes<range[2]),]
  }    
  tmp  
}
RawData=function(dfm,range=c(0,0)) {
  tmp<-dfm$RawData
  if(sum(range)!=0) {
    tmp<- tmp[(tmp$Minutes>range[1]) & (tmp$Minutes<range[2]),]
  }    
  tmp
}
CleanDFM<-function(){
  tmp<-ls(pattern="DFM.",pos=1)
  rm(list=tmp,pos=1)
  tmp<-ls(pattern="DFM..",pos=1)
  rm(list=tmp,pos=1)
}
ChangeParameterObject<-function(dfm,newP) {
  p<-dfm$Parameters
  baseline.flag<-FALSE
  threshold.flag<-FALSE
  eventpi.flag<-FALSE
  tmp.O<-options()
  options(warn=-1)
  dfm$Parameters<-newP
  ## Change only those that are listed
  if(p$Baseline.Window.Minutes!=newP$Baseline.Window.Minutes) {    
    baseline.flag<-TRUE
  }
  if(p$Feeding.Threshold!=newP$Feeding.Threshold) {    
    threshold.flag<-TRUE
  }
  if(p$Feeding.Minimum!=newP$Feeding.Minimum) {    
    threshold.flag<-TRUE
  }
  if(p$Tasting.Minimum!=newP$Tasting.Minimum) {    
    threshold.flag<-TRUE
  }
  if(p$Tasting.Maximum!=newP$Tasting.Maximum) {    
    threshold.flag<-TRUE
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
  
  if(p$PI.Multiplier!=newP$PI.Multiplier){
    eventpi.flag<-TRUE
  }
  
  ## Now update the stats needed
  if(baseline.flag==TRUE) {
    dfm<-CalculateBaseline(dfm)
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

GetDFM<-function(id){
  if (!is.numeric(id) || !all(is.finite(id)))
    stop("invalid arguments")
  
  ## Check to determine whether the DFM object already exists
  st<-paste("DFM",id,sep="")
  data<-NA
  if(exists(st,where=1)) {
    data<-get(st)  
  }
  data
}
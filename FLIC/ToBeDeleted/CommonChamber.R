

## Will output Feeding.Summary data for each chamber in each monitor
## over the specified range to a .csv file.
Feeding.Summary.Monitors<-function(monitors,parameters,range=c(0,0)){
  pnames<-Get.Parameter.Names(parameters)
  parameter.vector<-matrix(GetParameterVector(parameters),nrow=1)
  
  for(j in monitors){
    print(paste("Summarizing feeding data for DFM ",j,".",sep=""))
    flush.console()
    monitor<-j
    
    dfm<-DFMClass(monitor,parameters)  
    tmp<-Feeding.Summary(dfm,range)      
    tmp2<-data.frame(tmp,parameter.vector)
    names(tmp2)<-c(names(tmp),pnames)
    if(j==monitors[1]){
        results<-tmp2
      }
      else {
        results<-rbind(results,tmp2)  
      }
      
  }  
  filename<-paste("FeedingSummary_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
  write.csv(results,file=filename,row.names=FALSE)
}

Feeding.Summary<-function(dfm,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size==1)
    Feeding.Summary.OneWell(dfm,range)
  else if(dfm$Parameters$Chamber.Size==2)
    Feeding.Summary.TwoWell(dfm,range)
  else
    stop("Feeding Summary not implemented for this DFM type.")    
}

## This function will output the baselined (and cleaned) analog
## values (along with minutes, parameter values, etc) to a 
## .csv file.
OutputBaselinedData.DFM<-function(dfm, range=c(0,0)){
  
  parameter.vector<-GetDFMParameterVector(dfm)
  pnames<-Get.Parameter.Names(dfm$Parameters)
  
  tmp.all<-BaselineData(dfm,range)
  
  tmp<-data.frame(cbind(rep(dfm$ID,nrow(tmp.all)),tmp.all))
  tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
  
  tmp3<-cbind(tmp,tmp2)
  final.names<-c("DFM",names(tmp.all),pnames)
  names(tmp3)<-final.names
  
  filename<-paste("BaselinedData_DFM",dfm$ID,".csv",sep="") 
  write.csv(tmp3,file=filename,row.names=FALSE)  
}

## These functions will output the intervals
GetIntervalData.Well<-function(dfm,well, range=c(0,0)){
  nameA<-paste("W",well,sep="")
  parameter.vector<-GetDFMParameterVector(dfm)
  pnames<-Get.Parameter.Names(dfm$Parameters)
  
  theData<-dfm$Intervals[[nameA]]
  
  tmpA<-data.frame(rep(well,nrow(theData)),theData)
  names(tmpA)<-c("Well","Minutes","Sample","IntervalSec")
  
  tmp<-data.frame(cbind(rep(dfm$ID,nrow(tmpA)),tmpA))
  tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
  
  tmp3<-cbind(tmp,tmp2)
  names(tmp3)<-c("DFM","Well","Minutes","Sample","IntervalSec",pnames)
  
  tmp3
}
GetIntervalData.DFM<-function(dfm,range){
  for(i in 1:12){
    tmp<-GetIntervalData.Well(dfm,i)
    if(i==1)
      result<-tmp
    else
      result<-rbind(result,tmp)  
  }
  result
}

## This function will output the baselined (and cleaned) analog
## values (along with minutes, parameter values, etc) to a 
## separate .csv file for each chamber in each of the specified monitors.
OutputBaselinedData.Monitors<-function(monitors,parameters,range=c(0,0)){
  for(j in monitors){
    print(paste("Outputting Baselined Data for DFM ",j,".",sep=""))
    flush.console()
    monitor<-j
    dfm<-DFMClass(monitor,parameters)
    OutputBaselinedData.DFM(dfm,range)
    }
  }

OutputIntervalData.Monitors<-function(monitors,parameters,range=c(0,0)){
  for(j in monitors){
    print(paste("Outputting Interval Data for DFM ",j,".",sep=""))
    flush.console()
    monitor<-j
    dfm<-DFMClass(monitor,parameters)
    tmp2<-GetIntervalData.DFM(dfm,range)
    if(j==monitors[1]){
      result<-tmp2
    }
    else {
      result<-rbind(result,tmp2)
    }
  }
  filename<-paste("IntervalData_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}
## This fucntion will output for each well in each chamber for each monitor
## the total amount of time spend drinking over the perscribed range.
OutputTotalFeeding.Monitors<-function(monitors,parameters,range=c(0,0)){
  parameter.vector<-GetParameterVector(parameters)
  pnames<-Get.Parameter.Names(parameters)
  
  
  for(j in monitors){
    print(paste("Outputting TotalFeeding Data for DFM ",j,".",sep=""))
    flush.console()
    monitor<-j
    x<-1:12
    dfm<-DFMClass(monitor,parameters)
    tmp<-Feeding.Summary(dfm,range)
    if(parameters$Chamber.Size==1){
      atotal<-tmp$Events*tmp$MeanDuration
      d<-tmp$DFM
      c<-tmp$Well
      tmp2<-matrix(rep(parameter.vector,length(d)),ncol=length(parameter.vector),byrow=TRUE)
      tmp3<-data.frame(d,c,atotal,tmp2)
      names(tmp3)<-c("DFM","Well","TotalSec",pnames)
      if(j==monitors[1]){
        result<-tmp3
      }
      else {
        result<-rbind(result,tmp3)  
      }
    }
    else if(parameters$Chamber.Size==2){
      atotal<-tmp$EventsA*tmp$MeanDurationA
      btotal<-tmp$EventsB*tmp$MeanDurationB
      d<-tmp$DFM
      c<-tmp$Chamber
      tmp2<-matrix(rep(parameter.vector,length(d)),ncol=length(parameter.vector),byrow=TRUE)
      tmp3<-data.frame(d,c,atotal,btotal,tmp2)
      names(tmp3)<-c("DFM","Chamber","ATotalSec","BTotalSec",pnames)
      if(j==monitors[1]){
        result<-tmp3
      }
      else {
        result<-rbind(result,tmp3)  
      }      
    }
    else 
      stop("Feeding Summary not implemented for this DFM type.")    
  }
  filename<-paste("TotalFeedingTime_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}



###########################################
## These have to go here.
Feeding.Summary.OneWell<-function(dfm,range=c(0,0)){
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
  names(result)<-c("DFM","Well","Licks","Events","MeanDuration","MedDuration",
                   "MeanTimeBtw","MedTimeBtw","MeanInt","MedianInt","StartMin","EndMin")
  result    
  
}
Feeding.Summary.TwoWell<-function(dfm,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  cnames<-paste("C",1:nrow(dfm$Parameters$Chamber.Sets),sep="")
  for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
    wellA<-dfm$Parameters$Chamber.Sets[i,1]
    wellB<-dfm$Parameters$Chamber.Sets[i,2] 
    
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
    
    FPIs<-c(Feeding.FinalPI.Chamber(dfm,i,range),Feeding.FinalEventPI.Chamber(dfm,i,range))
    
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
  result    
  
}
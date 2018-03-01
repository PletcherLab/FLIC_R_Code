source("FLIC_Class_V2_Public.R")

## Functions that act on chambers
## These functions can be used to produce plots or to
## return data structures following analyses of single
## chambers.  Remember that the analysis parameters are 
## determined by the 'parameters' component of the
## chamber list. Note that some functions produce multiple
## plots so the plot page should be formatted accordingly 
## before the functions are called.


## Produces one plot
## Shows final PI for a range of fixed thresholds
## Thresholds must be a matrix with two columns.  Col 1 is feeding min
## Col 2 is threshold
Feeding.PICurve.FixedThresholds<-function(chamber,thresholds,range=c(0,0)){
  if(!is.matrix(thresholds)){
    stop("Thresholds must be a two column matrix")
  }
  if(ncol(thresholds)!=2){
    stop("Thresholds must be a two column matrix")
  }
  results<-matrix(rep(-1,nrow(thresholds)*5),ncol=5)
  for(i in 1:nrow(thresholds)){
    chamber<-ChangeParameter(chamber,Feeding.Threshold.Value=thresholds[i,1],Feeding.Interval.Min=thresholds[i,2])
    tmp<-FeedingData(chamber,range)
    results[i,]<-c(Feeding.FinalPI(chamber,range),sum(tmp$FeedingLicksA),sum(tmp$FeedingLicksB),thresholds[i,])
  }
  xs<-1:nrow(results)
  plot(xs,results[,1],ylim=c(-1,1),ylab="PI",xlab="Threshold",type="l")
  counts<-results[,2]+results[,3]
  positions<-rep(3,length(counts))
  positions[results[,1]>0.5]<-1
  text(xs,results[,1],as.character(counts),pos=positions,offset=1)
  title(paste("M:",chamber$Monitor," C:",chamber$Chamber," - PICurve"))
  tmp<-data.frame(results)
  names(tmp)<-c("FinalPI","FeedingLicksA","FeedingLicksB","Threshold","IntervalMin")
  tmp
}

## Produces one plot
## Shows final EventPI for a range of fixed thresholds
Feeding.EventPICurve.FixedThresholds<-function(chamber,thresholds,range=c(0,0)){
  if(!is.matrix(thresholds)){
    stop("Thresholds must be a two column matrix")
  }
  if(ncol(thresholds)!=2){
    stop("Thresholds must be a two column matrix")
  }  
  results<-matrix(rep(-1,nrow(thresholds)*5),ncol=5)
  for(i in 1:nrow(thresholds)){
    chamber<-ChangeParameter(chamber,Feeding.Threshold.Value=thresholds[i,1],Feeding.Interval.Min=thresholds[i,2])
    tmp<-FeedingData(chamber,range)
    results[i,]<-c(Feeding.FinalEventPI(chamber,range),sum(tmp$AEvents),sum(tmp$BEvents),thresholds[i,])
  }    
  xs<-1:nrow(results)
  plot(xs,results[,1],ylim=c(-1,1),ylab="PI",xlab="Threshold",type="l")
  counts<-results[,2]+results[,3]
  positions<-rep(3,length(counts))
  positions[results[,1]>0.5]<-1
  text(xs,results[,1],as.character(counts),pos=positions,offset=1)
  title(paste("M:",chamber$Monitor," C:",chamber$Chamber," - Feeding EventPICurve"))
  flush.console()
  tmp<-data.frame(results)
  names(tmp)<-c("FinalPI","EventsA","EventsB","Threshold","IntervalMin")
  tmp
}


## Functions that act on monitors
## These functions will output pdf files and csv files containing graphs and
## excel formatted data respectively.
## They will not output plots to a window so no window parameters
## need to be set.

OutputSecToFirstFeeding.Monitors<-function(monitors,parameters){
  parameter.vector<-GetParameterVector(parameters)
  pnames<-Get.Parameter.Names()
  for(j in 1:length(monitors)){
    print(paste("Outputting SecToFirstFeeding Data for monitor ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    x<-1:6
    for(i in x){
      flush.console()
      c<-ChamberClass(monitor,i,parameters)
      tmp2<-SecondsToFirstFeeding(c)      
      tmp3<-c(Monitor(c),Chamber(c),tmp2,parameter.vector)
      names(tmp3)<-c("Monitor","Chamber",names(tmp2),pnames)
      if(i==1 && j==1){
        result<-tmp3
      }
      else {
        result<-rbind(result,tmp3)  
      }      
    }
  }
  filename<-paste("SecToFirstFeeding_M",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE) 
}

OutputSecBtwFeedings.Monitors<-function(monitors,parameters,high.event){
  parameter.vector<-GetParameterVector(parameters)
  pnames<-Get.Parameter.Names()
  for(j in 1:length(monitors)){
    print(paste("Outputting SecBtwFeedings Data for monitor ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    x<-1:6
    for(i in x){
      flush.console()
      c<-ChamberClass(monitor,i,parameters)
      tmp2<-SecondsBtwFeedings(c,high.event)      
      tmp3<-c(Monitor(c),Chamber(c),high.event,tmp2,parameter.vector)
      names(tmp3)<-c("Monitor","Chamber","FeedingNum",names(tmp2),pnames)
      if(i==1 && j==1){
        result<-tmp3
      }
      else {
        result<-rbind(result,tmp3)  
      }      
    }
  }
  filename<-paste("SecBtwFeedings_M",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}


#################################################################################
## Below here are special functions are almost surely will not work in a general
## situation.  They are constructed just to get us through particular analyses.


## I think that this is written to look at feeding around a specific window size starting
## at the start time and looking every 12 hours for a specificed number of intervals.
GetIntegrationAreas.Chamber<-function(chamber,starttime.sec,intervalnum,window.min) {
  ## This will increment 
  starttime.min<-starttime.sec/60
  min.12hr<-60*12
  result<-data.frame(matrix(rep(-1,intervalnum*9),ncol=9))
  names(result)<-c("Monitor","Chamber","Transition.Min","LicksA","LicksB","AvgIntA","AvgIntB","SumA","SumB")
  transition.points<-seq(from=starttime.min,by=min.12hr,len=intervalnum)
  for(i in 1:length(transition.points)){
    window.range<-c(transition.points[i]-window.min,transition.points[i]+window.min)
    licks<-Feeding.TotalLicks(chamber,window.range)
    int<-Feeding.IntensitySummary(chamber,window.range)
    int<-c(int$MeanIntA,int$MeanIntB)
    total<-licks*int
    result[i,]<-c(Monitor(chamber),Chamber(chamber),transition.points[i],licks,int,total)
  }
  result
}

OutputIntegrationAreas.Monitors<-function(monitors,parameters,starttime.sec,intervalnum,window.min){
  tmp3<-data.frame(matrix(rep(1,9),nrow=1))
  names(tmp3)<-c("Monitor","Chamber","Transition.Min","LicksA","LicksB","AvgIntA","AvgIntB","SumA","SumB")
  for(j in 1:length(monitors)){
    print(paste("Outputting Integration Data for monitor ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    x<-1:6
    for(i in x){
      flush.console()
      c<-ChamberClass(monitor,i,parameters)
      tmp2<-GetIntegrationAreas.Chamber(c,starttime.sec,intervalnum,window.min)
      tmp3<-rbind(tmp3,tmp2)
    }
  }
  tmp3<-tmp3[-1,]
  filename<-paste("IntegrationData_M",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(tmp3,file=filename,row.names=FALSE)  
}

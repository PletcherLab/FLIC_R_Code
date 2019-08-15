source("ExpDesignFunctions.R")
require(ggplot2)
require(reshape2)

#####Treatment based functions######
## Treatment based functions
## Divisions will create a separate graph for the cumulative PI (starting at range[0]) up
## to each of the division points in the experiment.  This is distinct from the time-dependent
## PI because it will always start from the first point in the data set (satisfying the first
## range parameter).

## Although it is not optimal, the two-well aspect of these functions just adds or averages the values of
## both wells.  In the future this should probably present values for each well, adjusted for the PI multiplier.
Feeding.LicksPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingLicks.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    FeedingLicks.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
Feeding.EventsPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingEvents.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    Feeding.Events.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
Feeding.MeanDurationsPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingMeanDuration.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    FeedingMeanDuration.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
Feeding.MeanTimeBtwPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingMeanTimeBtw.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    FeedingMeanTimeBtw.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
################################

###################################
## Will output Feeding.Summary data for each chamber in each monitor
## over the specified range to a .csv file.
Feeding.Summary.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0),SaveToFile=TRUE,TransformLicks=TRUE){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  for(j in 1:length(monitors)){
    ##print(paste("Summarizing feeding data for DFM ",monitors[j],".",sep=""))
    ##flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)  
    parameter.vector<-matrix(GetParameterVector(p),nrow=1)
    pnames<-Get.Parameter.Names(p)
    tmp<-Feeding.Summary(dfm,range,TransformLicks)      
    tmp2<-data.frame(tmp,parameter.vector)
    names(tmp2)<-c(names(tmp),pnames)
    if(j==1){
        results<-tmp2
      }
      else {
        results<-rbind(results,tmp2)  
      }
      
  }  
  if(is.data.frame(expDesign)) {
    results<-AppendTreatmentonResultsFrame(results,expDesign)
    trt.summary<-suppressWarnings(AggregateTreatments(results))
    if(SaveToFile==TRUE){
      filename<-paste("FeedingSummary_TRT_Stats",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(trt.summary,file=filename,row.names=FALSE)
      filename<-paste("FeedingSummary_TRT_Data",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(results,file=filename,row.names=FALSE)
    }
  }
  else if(SaveToFile==TRUE){
    filename<-paste("FeedingSummary_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
  }
  
  if(is.data.frame(expDesign)) {
    return(list(Results=results,Stats=trt.summary))
  }
  else {
    return(list(Results=results))
  }
}
BinnedFeeding.Summary.Monitors<-function(monitors,parameters,binsize.min=30,expDesign=NA,range=c(0,0),SaveToFile=TRUE,TransformLicks=TRUE){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  for(j in 1:length(monitors)){
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)  
    parameter.vector<-matrix(GetParameterVector(p),nrow=1)
    pnames<-Get.Parameter.Names(p)
    tmp<-BinnedFeeding.Summary(dfm,binsize.min,range,TransformLicks)      
    tmp2<-data.frame(tmp,parameter.vector)
    names(tmp2)<-c(names(tmp),pnames)
    if(j==1){
      results<-tmp2
    }
    else {
      results<-rbind(results,tmp2)  
    }
    
  }  
  if(is.data.frame(expDesign)) {
    results<-AppendTreatmentonResultsFrame(results,expDesign)
    trt.summary<-suppressWarnings(AggregateTreatmentsBinnedData(results))
    if(SaveToFile==TRUE){
      filename<-paste("BinnedSummary_TRT_Stats",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(trt.summary,file=filename,row.names=FALSE)
      filename<-paste("BinnedSummary_TRT_Data",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(results,file=filename,row.names=FALSE)
    }
  }
  else if(SaveToFile==TRUE){
    filename<-paste("BinnedSummary_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
  }
  
  if(is.data.frame(expDesign)) {
    return(list(Results=results,Stats=trt.summary))
  }
  else {
    return(list(Results=results))
  }
}

Feeding.Summary<-function(dfm,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size==1)
    Feeding.Summary.OneWell(dfm,range,TransformLicks)
  else if(dfm$Parameters$Chamber.Size==2)
    Feeding.Summary.TwoWell(dfm,range,TransformLicks)
  else
    stop("Feeding Summary not implemented for this DFM type.")    
}
BinnedFeeding.Summary<-function(dfm,binsize.min=30,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size==1)
    BinnedFeeding.Summary.OneWell(dfm,binsize.min,range,TransformLicks)
  else if(dfm$Parameters$Chamber.Size==2)
    BinnedFeeding.Summary.TwoWell(dfm,binsize.min,range,TransformLicks)
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
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  for(j in 1:length(monitors)){
    print(paste("Outputting Baselined Data for DFM ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    OutputBaselinedData.DFM(dfm,range)
    }
  }

OutputIntervalData.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  for(j in 1:length(monitors)){
    ##print(paste("Outputting Interval Data for DFM ",monitors[j],".",sep=""))
    ##flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    tmp2<-GetIntervalData.DFM(dfm,range)
    if(is.data.frame(expDesign))
      tmp2<-AppendTreatmentonResultsFrame(tmp2,expDesign)
    if(j==1){
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
OutputTotalFeeding.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  for(j in 1:length(monitors)){
    print(paste("Outputting TotalFeeding Data for DFM ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    x<-1:12
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    parameter.vector<-GetParameterVector(p)
    pnames<-Get.Parameter.Names(p)
    tmp<-Feeding.Summary(dfm,range)
    if(p$Chamber.Size==1){
      atotal<-tmp$Events*tmp$MeanDuration
      d<-tmp$DFM
      c<-tmp$Chamber
      tmp2<-matrix(rep(parameter.vector,length(d)),ncol=length(parameter.vector),byrow=TRUE)
      tmp3<-data.frame(d,c,atotal,tmp2)
      names(tmp3)<-c("DFM","Chamber","TotalSec",pnames)
      if(j==1){
        result<-tmp3
      }
      else {
        result<-rbind(result,tmp3)  
      }
    }
    else if(p$Chamber.Size==2){
      atotal<-tmp$EventsA*tmp$MeanDurationA
      btotal<-tmp$EventsB*tmp$MeanDurationB
      d<-tmp$DFM
      c<-tmp$Chamber
      tmp2<-matrix(rep(parameter.vector,length(d)),ncol=length(parameter.vector),byrow=TRUE)
      tmp3<-data.frame(d,c,atotal,btotal,tmp2)
      names(tmp3)<-c("DFM","Chamber","ATotalSec","BTotalSec",pnames)
      if(j==1){
        result<-tmp3
      }
      else {
        result<-rbind(result,tmp3)  
      }      
    }
    else 
      stop("Feeding Summary not implemented for this DFM type.")    
  }
  
  if(is.data.frame(expDesign))
    result<-AppendTreatmentonResultsFrame(result,expDesign)
  
  filename<-paste("TotalFeedingTime_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}

RawDataPlot.DFM<-function(dfm,range=c(0,0),OutputPNGFile=FALSE) {
  ##windows(record=FALSE,width=8,height=12) # opens a window and starts recording
  tmp<-RawData(dfm,range)
  tmp<-tmp[,c(1,7,8,9,10,11,12,13,14,15,16,17,18)]
  thresh.high<-dfm$Parameters$Feeding.Threshold
  thresh.low<-dfm$Parameters$Feeding.Minimum
  tmp.m<-melt(tmp, id="Minutes")
  gp<-ggplot(data = tmp.m, aes(x = Minutes, y = value)) +
    geom_line() + facet_grid(variable ~ .) + ggtitle(paste("DFM:",dfm$ID))
  fn<-paste("RawDataPlot_DFM",dfm$ID,".png",sep="")
  if(OutputPNGFile==TRUE)
    ggsave(filename = fn)
  else
    show(gp)
}
BaselinedDataPlot.DFM<-function(dfm,range=c(0,0),OutputPNGFile=FALSE, IncludeThresholds=FALSE) {
  ##windows(record=FALSE,width=8,height=12) # opens a window and starts recording
  tmp<-BaselineData(dfm,range)
  tmp<-tmp[,c(1,7,8,9,10,11,12,13,14,15,16,17,18)]
  thresh.high<-dfm$Parameters$Feeding.Threshold
  thresh.low<-dfm$Parameters$Feeding.Minimum
  tmp.m<-melt(tmp, id="Minutes")
  gp<-ggplot(data = tmp.m, aes(x = Minutes, y = value)) +
    geom_line() + facet_grid(variable ~ .) + ggtitle(paste("DFM:",dfm$ID))
  if(IncludeThresholds==TRUE)
    gp<-gp+
    geom_hline(yintercept=thresh.high,linetype="dashed", color = "red", size=0.2) + 
    geom_hline(yintercept=thresh.low,linetype="dashed", color = "red", size=0.2) 
  fn<-paste("BaselinedDataPlot_DFM",dfm$ID,".png",sep="")
  if(OutputPNGFile==TRUE)
    ggsave(filename = fn)
  else
    show(gp)
}

BinnedLicksPlot.DFM<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size==1)
    PlotBins.Licks.DFM.OneWell(dfm,binsize.min,range,TransformLicks)
  else if(dfm$Parameters$Chamber.Size==2)
    PlotBins.Licks.DFM.TwoWell(dfm,binsize.min,range,TransformLicks)
  else
    stop("Binned lick plots not implemented for this DFM type.")    
}


#################################################################
#####Private Functions that should not be called by user ########
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
      dfm<-DFMClass(monitors[1],parameters)
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
          r<-paste("Transformed Licks -- Range(min): (",range[1],",",range[2],")",sep="")
          ylabel<-"Transformed Licks"
        }
        else {
          r<-paste("Licks -- Range(min): (",range[1],",",range[2],")",sep="")
          ylabel<-"Licks"
        }
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
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
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
    r<-paste("Events -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
    print(summary(aov(Events~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("Events -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
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
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
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
      r<-paste("Transformed Licks -- Range(min): (",range[1],",",range[2],")",sep="")
      ylabel<-"Transformed Licks"
    }
    else {
      r<-paste("Licks -- Range(min): (",range[1],",",range[2],")",sep="")
      ylabel<-"Licks"
    }
    results<-data.frame(results,Licks)
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
          r<-paste("Transformed Licks -- Range(min): (",range[1],",",range[2],")",sep="")
          ylabel<-"Transformed Licks"
          Licks<-Licks^0.25
        }
        else {
          r<-paste("Licks -- Range(min): (",range[1],",",range[2],")",sep="")
          ylabel<-"Licks"
        }
        results<-data.frame(results,Licks)
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
  print(summary(aov(Licks~Treatment,data=results)))
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
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
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
    Licks<-results$LicksA+results$LicksB
    results<-data.frame(results,Licks)
    r<-paste("Events -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        Licks<-results$LicksA+results$LicksB
        results<-data.frame(results,Licks)
        r<-paste("Events -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
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
  print(summary(aov(Events~Treatment,data=results)))
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
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
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
            ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Duration") + guides(fill=FALSE))
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
                    ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Duration") + guides(fill=FALSE))
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
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
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
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
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
            ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Duration") + guides(fill=FALSE))
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
                    ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Duration") + guides(fill=FALSE))
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
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
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

BinnedFeeding.Summary.OneWell<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for single chambers only")
  result<-BinLicks.Well(dfm,1,binsize.min,range)
  tmp<-BinEvents.Well(dfm,1,binsize.min,range)
  Events<-tmp$Events
  result<-data.frame(result,Events)
  Well<-factor(rep(1,nrow(result)))
  DFM<-factor(rep(dfm$ID,nrow(result)))
  result<-data.frame(result,DFM,Well)
  
  for(i in 2:12) {
    tmp<-BinLicks.Well(dfm,i,binsize.min,range)
    tmp2<-BinEvents.Well(dfm,i,binsize.min,range)
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
BinnedFeeding.Summary.TwoWell<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE){
  
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  wellA<-dfm$Parameters$Chamber.Sets[1,1]
  wellB<-dfm$Parameters$Chamber.Sets[1,2] 
  
  ltmp<-BinLicks.Well(dfm,wellA,binsize.min,range)
  ltmp2<-BinLicks.Well(dfm,wellB,binsize.min,range)
  etmp<-BinEvents.Well(dfm,wellA,binsize.min,range)
  etmp2<-BinEvents.Well(dfm,wellB,binsize.min,range)
  Chamber<-factor(rep(1,nrow(ltmp)))
  DFM<-factor(rep(dfm$ID,nrow(ltmp)))
  
  result<-data.frame(ltmp,ltmp2$Licks,etmp$Events,etmp2$Events,DFM,Chamber)
  
  for(i in 2:nrow(dfm$Parameters$Chamber.Sets)) {
    wellA<-dfm$Parameters$Chamber.Sets[i,1]
    wellB<-dfm$Parameters$Chamber.Sets[i,2] 
    ltmp<-BinLicks.Well(dfm,wellA,binsize.min,range)
    ltmp2<-BinLicks.Well(dfm,wellB,binsize.min,range)
    etmp<-BinEvents.Well(dfm,wellA,binsize.min,range)
    etmp2<-BinEvents.Well(dfm,wellB,binsize.min,range)
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
BinEvents.Well<-function(dfm,well,binsize.min,range=c(0,0)){
  tmp<-FeedingData.Events(dfm,range)
  cname=paste("W",well,sep="")
  
  tmp<-tmp[,c("Minutes",cname)]
  ## Remember that Event data include duration, but we aren't interested
  ## in that.  Set values >0 to 1.
  tmp[tmp[,cname]>1,cname]<-1
  
  m.min<-min(tmp$Minutes)
  m.max<-max(tmp$Minutes)
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  z<-cut(tmp$Minutes,y,include.lowest=TRUE)
  
  r.min<-aggregate(tmp$Minutes~z,FUN=mean)
  r.A<-aggregate(tmp[,cname]~z,FUN=sum)
  
  results<-data.frame(r.min,r.A[,2])
  names(results)<-c("Interval","Min","Events")
  results
}
BinLicks.Well<-function(dfm,well,binsize.min,range=c(0,0)){
  tmp<-FeedingData.Licks(dfm,range)
  cname=paste("W",well,sep="")
  
  tmp<-tmp[,c("Minutes",cname)]
  
  m.min<-min(tmp$Minutes)
  m.max<-max(tmp$Minutes)
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  z<-cut(tmp$Minutes,y,include.lowest=TRUE)
  
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
  trt.summary[,tmp]
}
AggregateTreatments<-function(results){
  trt.summary1<-aggregate(results,by=list(results$Treatment),mean) 
  trt.summary2<-aggregate(results,by=list(results$Treatment),mySEM)
  trt.summary1<-trt.summary1[,-grep("Treatment|DFM|Chamber",colnames(trt.summary1))]
  trt.summary2<-trt.summary2[,-grep("Treatment|DFM|Chamber",colnames(trt.summary2))]
  
  names(trt.summary1)[names(trt.summary1) == "Group.1"] <- "Treatment"
  tmp<-names(trt.summary1)[2:19]
  tmp<-paste(tmp,"SEM",sep="")
  
  trt.summary<-data.frame(trt.summary1[,1:19],trt.summary2[,2:19],trt.summary1[,20:ncol(trt.summary1)])
  names(trt.summary)<-c(names(trt.summary1)[1:19],tmp,names(trt.summary1)[20:ncol(trt.summary1)])
  
  tmp<-c("Treatment","MeanLicks","MeanEvents","MeanMDuration","MeanMedDuration","MeanMTimeBtw","MeanMedTimeBtw","MeanMInt","MeanMedInt",
         "SEMLicks","SEMEvents","SEMMDuration","SEMMedDuration","SEMMTimeBtw","SEMMedTimeBtw","SEMMInt","SEMMedInt",names(trt.summary)[18:ncol(trt.summary)])
  
  names(trt.summary)<-tmp
  trt.summary
}
PlotBins.Licks.DFM.OneWell<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for single chambers only")
  binnedData<-BinnedFeeding.Summary(dfm,binsize.min,range,TransformLicks)
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
  binnedData<-BinnedFeeding.Summary(dfm,binsize.min,range,TransformLicks)
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


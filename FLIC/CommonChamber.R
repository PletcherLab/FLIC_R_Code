source("PrivateFunctions.R")
require(ggplot2)
require(reshape2)

##### QC Functions ######
OutputBaselinedData.DFM<-function(dfm, range=c(0,0),filename="Baselined"){
  
  parameter.vector<-GetDFMParameterVector(dfm)
  pnames<-Get.Parameter.Names(dfm$Parameters)
  
  tmp.all<-BaselineData(dfm,range)
  
  tmp<-data.frame(cbind(rep(dfm$ID,nrow(tmp.all)),tmp.all))
  tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
  
  tmp3<-cbind(tmp,tmp2)
  final.names<-c("DFM",names(tmp.all),pnames)
  names(tmp3)<-final.names
  
  filename<-paste(filename,"_DFM",dfm$ID,".csv",sep="") 
  write.csv(tmp3,file=filename,row.names=FALSE)  
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

##### Experiment Analysis Functions ######
Feeding.Summary.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0),SaveToFile=TRUE,TransformLicks=TRUE,filename="FeedingSummary"){
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
    tmp<-Feeding.Summary.DFM(dfm,range,TransformLicks)      
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
      filename2<-paste(filename,"_TRT_Stats",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(trt.summary,file=filename2,row.names=FALSE)
      filename2<-paste(filename,"_TRT_Data",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(results,file=filename2,row.names=FALSE)
    }
  }
  else if(SaveToFile==TRUE){
    filename<-paste(filename,"_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
  }
  
  if(is.data.frame(expDesign)) {
    return(list(Results=results,Stats=trt.summary))
  }
  else {
    return(list(Results=results))
  }
}
BinnedFeeding.Summary.Monitors<-function(monitors,parameters,binsize.min=30,expDesign=NA,range=c(0,0),SaveToFile=TRUE,TransformLicks=TRUE,filename="BinnedSummary"){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  ## If there is no range specified, choose the last time for all monitors.
  if(sum(range)==0) {
    EndTimeMin<-0
    for(j in 1:length(monitors)){
      monitor<-monitors[j]
      if(individ.params==TRUE)
        p<-parameters[[j]]
      else
        p<-parameters
      dfm<-DFMClass(monitor,p)   
      tmp<-LastSampleData(dfm)$Minutes
      if(tmp>EndTimeMin)
        EndTimeMin<-tmp
    }
    range[2]<-EndTimeMin
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
    tmp<-BinnedFeeding.Summary.DFM(dfm,binsize.min,range,TransformLicks)      
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
      filename2<-paste(filename,"_TRT_Stats",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(trt.summary,file=filename2,row.names=FALSE)
      filename2<-paste(filename,"_TRT_Data",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(results,file=filename2,row.names=FALSE)
    }
  }
  else if(SaveToFile==TRUE){
    filename<-paste(filename,"_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
  }
  
  if(is.data.frame(expDesign)) {
    return(list(Results=results,Stats=trt.summary))
  }
  else {
    return(list(Results=results))
  }
}


##### Simple Experiment Plotting Functions ######
BinnedDataPlot<-function(binnedDataResult,Type="Licks",SaveToFile=FALSE){
  if("LicksA" %in% names(binnedDataResult$Results)){
    BinnedPlot.TwoWell.Trt(binnedDataResult,Type,SaveToFile)
  }
  else {
    BinnedPlot.OneWell.Trt(binnedDataResult,Type,SaveToFile)
  }
}
DataPlot<-function(summaryResults,Type="Licks",SaveToFile=FALSE){
  if("LicksA" %in% names(binnedDataResult$Results)){
    SimpleDataPlot.TwoWell(summaryResults,Type,SaveToFile)
  }
  else {
    SimpleDataPlot.OneWell(summaryResults,Type,SaveToFile)
  }
}

##### Divided Box Plots ######




#####Treatment based functions######
## Treatment based functions
## Divisions will create a separate graph for the cumulative PI (starting at range[0]) up
## to each of the division points in the experiment.  This is distinct from the time-dependent
## PI because it will always start from the first point in the data set (satisfying the first
## range parameter).

## Although it is not optimal, the two-well aspect of these functions just adds or averages the values of
## both wells.  In the future this should probably present values for each well, adjusted for the PI multiplier.


Feeding.LicksPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE,TransformLicks=TRUE){
  if(is.list(parameters[[1]])){
    cs <- parameters[[1]]$Chamber.Size
  }
  else {
    cs<-parameters$Chamber.Size
  }
  if(cs==1)
    FeedingLicks.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(cs==2)
    FeedingLicks.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
Feeding.EventsPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(is.list(parameters[[1]])){
    cs <- parameters[[1]]$Chamber.Size
  }
  else {
    cs<-parameters$Chamber.Size
  }
  if(cs==1)
    FeedingEvents.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(cs==2)
    FeedingEvents.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
Feeding.MeanDurationsPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(is.list(parameters[[1]])){
    cs <- parameters[[1]]$Chamber.Size
  }
  else {
    cs<-parameters$Chamber.Size
  }
  if(cs==1)
    FeedingMeanDuration.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(cs==2)
    FeedingMeanDuration.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
Feeding.MeanTimeBtwPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(is.list(parameters[[1]])){
    cs <- parameters[[1]]$Chamber.Size
  }
  else {
    cs<-parameters$Chamber.Size
  }
  if(cs==1)
    FeedingMeanTimeBtw.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(cs==2)
    FeedingMeanTimeBtw.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}


###################################
## Will output Feeding.Summary data for each chamber in each monitor
## over the specified range to a .csv file.


PlotTotalLicks.Monitors<-function(monitors, p, range=c(0,0),TransformLicks=TRUE){
  tmp2<-Feeding.Summary.Monitors(monitors,p,range=range,TransformLicks)$Results
  
  ylabel="Licks"
  if(TransformLicks==TRUE) {
    ylabel="Transformed Licks"
  }
  
  tmp2$DFM<-factor(tmp2$DFM)
  
  if("LicksA" %in% names(tmp2)){
    tmp2<-tmp2[,c("DFM","LicksA","LicksB")]
    tmp2<-melt(tmp2,id.vars="DFM",value.name="Licks", variable.name="Well")
    gp<-ggplot(tmp2,aes(x=DFM,y=Licks,fill=Well)) + geom_dotplot(binaxis='y',stackdir='center',stackratio=1.5, dotsize=0.7) +
      ylab(ylabel)
  }
  else {
    gp<-ggplot(tmp2,aes(x=DFM,y=Licks,fill=DFM)) + geom_dotplot(binaxis='y',stackdir='center',stackratio=1.5, dotsize=0.7) +
      ylab(ylabel)
  }
  gp
}
## This function will output the baselined (and cleaned) analog
## values (along with minutes, parameter values, etc) to a 
## separate .csv file for each chamber in each of the specified monitors.



OutputBaselinedData.Monitors<-function(monitors,parameters,range=c(0,0),filename="Baselined"){
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
    OutputBaselinedData.DFM(dfm,range,filename)
  }
}
OutputIntervalData.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0),filename="IntervalData"){
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
  filename<-paste(filename,"_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}


OutputDurationData.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0),filename="DurationsData"){
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
    tmp2<-GetDurationData.DFM(dfm,range)
    if(is.data.frame(expDesign))
      tmp2<-AppendTreatmentonResultsFrame(tmp2,expDesign)
    if(j==1){
      result<-tmp2
    }
    else {
      result<-rbind(result,tmp2)
    }
  }
  filename<-paste(filename,"_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}


## This fucntion will output for each well in each chamber for each monitor
## the total amount of time spend drinking over the perscribed range.
OutputTotalFeeding.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0),filename="TotalFeedingTime"){
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
    tmp<-Feeding.Summary.DFM(dfm,range)
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
  
  filename<-paste(filename,"_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}


##### Functions for Exploring Individual DFMs ######

Feeding.Summary.DFM<-function(dfm,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size==1)
    Feeding.Summary.OneWell(dfm,range,TransformLicks)
  else if(dfm$Parameters$Chamber.Size==2)
    Feeding.Summary.TwoWell(dfm,range,TransformLicks)
  else
    stop("Feeding Summary not implemented for this DFM type.")    
}
BinnedFeeding.Summary.DFM<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE){
  if(sum(range)!=0) {
    m.min<-range[1]
    m.max<-range[2]
  }    
  else{
    m.min<-0
    m.max<-max(dfm$RawData$Minutes)
  }
  if(m.min>m.max)
    m.max<-m.min+1
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  tmpMatrix<-cbind(y[-length(y)],y[-1])
  intervals<-cut(y+1,y,include.lowest=TRUE,dig.lab=8)
  intervals<-intervals[-length(intervals)]
  result<-Feeding.Summary.DFM(dfm,range=tmpMatrix[1,],TransformLicks)
  Interval<-rep(intervals[1],nrow(result))
  Minutes<-rep(mean(tmpMatrix[1,]),nrow(result))
  result<-data.frame(Interval,Minutes,result)
  for(i in 2:nrow(tmpMatrix)){
    tmp<-Feeding.Summary.DFM(dfm,range=tmpMatrix[i,],TransformLicks)
    Interval<-rep(intervals[i],nrow(tmp))
    Minutes<-rep(mean(tmpMatrix[i,]),nrow(tmp))
    tmp<-data.frame(Interval,Minutes,tmp)
    result<-rbind(result,tmp)
  }
  result
}

GetIntervalData.DFM<-function(dfm,range){
  for(i in 1:12){
    tmp<-GetIntervalData.Well(dfm,i,range)
    if(i==1)
      result<-tmp
    else
      result<-rbind(result,tmp)  
  }
  result
}
GetDurationData.DFM<-function(dfm,range){
  for(i in 1:12){
    tmp<-GetDurationData.Well(dfm,i,range)
    if(i==1)
      result<-tmp
    else
      result<-rbind(result,tmp)  
  }
  result
}

BinnedLicksPlot.DFM<-function(dfm,binsize.min=30,range=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size==1)
    PlotBins.Licks.DFM.OneWell(dfm,binsize.min,range,TransformLicks)
  else if(dfm$Parameters$Chamber.Size==2)
    PlotBins.Licks.DFM.TwoWell(dfm,binsize.min,range,TransformLicks)
  else
    stop("Binned lick plots not implemented for this DFM type.")    
}
BinnedDurationsPlot.DFM<-function(dfm,binsize.min=30,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size==1)
    PlotBins.Durations.DFM.OneWell(dfm,binsize.min,range)
  else if(dfm$Parameters$Chamber.Size==2)
    PlotBins.Durations.DFM.TwoWell(dfm,binsize.min,range)
  else
    stop("Binned durations plots not implemented for this DFM type.")    
}
CumulativeLicksPlots.DFM<-function(dfm,SinglePlot=FALSE,TransformLicks=TRUE){
  tmp<-Feeding.Durations.Well(dfm,1)
  SumLicks<-cumsum(tmp$Licks)
  if(TransformLicks==TRUE)
    SumLicks<-SumLicks^0.25
  row<-1
  col<-1
  Row<-rep(row,nrow(tmp))
  Col<-rep(col,nrow(tmp))
  Well<-rep(1,nrow(tmp))
  tmp<-data.frame(tmp,SumLicks,Row,Col,Well)
  for(i in 2:12){
    if(i%%2==1) row<-row+1
    col<-col+1
    if(col>2) col<-1
    tmp2<-Feeding.Durations.Well(dfm,i)
    if(is.data.frame(tmp2)) {
      x<-tmp2$Minutes
      y<-cumsum(tmp2$Licks)
      SumLicks<-y
      if(TransformLicks==TRUE)
        SumLicks<-SumLicks^0.25
      Row<-rep(row,length(x))
      Col<-rep(col,length(x))
      Well<-rep(i,length(x))
      tmp2<-data.frame(tmp2,SumLicks,Row,Col,Well)
      tmp<-rbind(tmp,tmp2) 
    }
    else {
      fake.entry1<-c(FirstSampleData(dfm)$Minutes,0,0,0,0,0,0,0,0,row,col,i)
      fake.entry2<-c(LastSampleData(dfm)$Minutes,0,0,0,0,0,0,0,0,row,col,i)
      fake.entry<-data.frame(rbind(fake.entry1,fake.entry2))
      names(fake.entry)<-names(tmp)
      tmp<-rbind(tmp,fake.entry) 
    }
  }
  if(dfm$Parameters$Chamber.Size==2){
    
    even<-(as.numeric(as.character(tmp$Well))%%2==0)
    if(dfm$Parameters$PI.Multiplier==1){
      WellTC<-rep("WellA",nrow(tmp))
      WellTC[even]<-"WellB"
    }
    else {
      WellTC<-rep("WellB",nrow(tmp))
      WellTC[even]<-"WellA"
    }
    tmp<-data.frame(tmp,WellTC)
  }
  
  if(TransformLicks==TRUE)
    ylabel="Transformed Cumulative Licks"
  else
    ylabel="Cumulative Licks"
  if(SinglePlot==FALSE) {
    if(dfm$Parameters$Chamber.Size==1) {
      gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line() + facet_grid(rows=vars(Row),cols=vars(Col)) +geom_point() +
        ggtitle(paste("DFM",dfm$ID)) + ylab(ylabel) + labs(color="Chamber")
    }
    else {
      gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line() + facet_grid(rows=vars(Row),cols=vars(WellTC)) +geom_point() +
        ggtitle(paste("DFM",dfm$ID)) + ylab(ylabel) + labs(color="Chamber")
    }
    
  }
  else {
    gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line(size=1.2) +
      ggtitle(paste("DFM",dfm$ID))+ ylab(ylabel)+ labs(color="Chamber")
  }
  gp
}


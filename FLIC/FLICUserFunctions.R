source("DFM.R")
source("QC.R")
require(ggplot2)
require(reshape2)
require(gtools)
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
## Use this function to identify large well- or DFM-based effects.
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
      filename2<-paste(filename,"_Stats.csv",sep="")
      write.csv(trt.summary,file=filename2,row.names=FALSE)
      filename2<-paste(filename,"_Data.csv",sep="")
      write.csv(results,file=filename2,row.names=FALSE)
    }
  }
  else if(SaveToFile==TRUE){
    filename<-paste(filename,".csv",sep="")
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
      filename2<-paste(filename,"_Stats.csv",sep="")
      write.csv(trt.summary,file=filename2,row.names=FALSE)
      filename2<-paste(filename,"_Data.csv",sep="")
      write.csv(results,file=filename2,row.names=FALSE)
    }
  }
  else if(SaveToFile==TRUE){
    filename<-paste(filename,".csv",sep="")
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
## These functions take the output of the experimental analysis functions and rapidly plot different measures.
## Current Type options are (case sensitive): Licks, Events, Durations, MinInt, TimeBtw, PI, EventPI (latter 2 for choice chambers only).
BinnedDataPlot<-function(binnedDataResult,Type="Licks",SaveToFile=FALSE){
  if("LicksA" %in% names(binnedDataResult$Results)){
    BinnedPlot.TwoWell.Trt(binnedDataResult,Type,SaveToFile)
  }
  else {
    BinnedPlot.OneWell.Trt(binnedDataResult,Type,SaveToFile)
  }
}

## Current Type options are (case sensitive): Licks, Events, Durations, MinInt, TimeBtw, PI, EventPI (latter 2 for choice chambers only).
DataPlot<-function(summaryResults,Type="Licks",SaveToFile=FALSE){
  if("LicksA" %in% names(summaryResults$Results)){
    SimpleDataPlot.TwoWell(summaryResults,Type,SaveToFile)
  }
  else {
    SimpleDataPlot.OneWell(summaryResults,Type,SaveToFile)
  }
}

##### Advanced Cumulative Division Box Plots ######
## Divisions will create a separate graph for the cumulative PI (starting at range[0]) up
## to each of the division points in the experiment.  This is distinct from the time-dependent
## PI because it will always start from the first point in the data set (satisfying the first
## range parameter).

## These plots allow the user to specify a range.

## Although it is not optimal, the two-well aspect of these functions just adds or averages the values of
## both wells.  In the future this should probably present values for each well, adjusted for the PI multiplier.

## These plots are slower than the simple plots because analysis calculations are required.
## Division plots are not optimized for choice chambers because it combines well data at the moment (4/23/2020).
## Current Type options are (case sensitive): Licks, Events, Durations, MinInt, TimeBtw, PI, EventPI (latter 2 for choice chambers only).
DivisionPlots<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,Type="Licks",SaveToFile=FALSE,TransformLicks=TRUE){
  if(is.list(parameters[[1]])){
    cs <- parameters[[1]]$Chamber.Size
  }
  else {
    cs<-parameters$Chamber.Size
  }
  if(cs==1)
    DivisionPlots.OneWell(monitors,parameters,expDesign,range,divisions,Type,SaveToFile,TransformLicks)
  else if(cs==2)
    DivisionPlots.TwoWell(monitors,parameters,expDesign,range,divisions,Type,SaveToFile,TransformLicks)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}

##### Cumulative plots for choice chambers only (Ugly) #####
CumulativePIPlots<-function(monitors,parameters,expDesign,range=c(0,0),SaveToFile=FALSE){
  individ.params<-FALSE
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  if(SaveToFile==TRUE){
    filename<-paste("CumulativePI.pdf",sep="")
    pdf(file=filename)
  }
  
  ## Basically plot the culuative PI plot for each chamber for each treatment
  ## Each Treatment will get a different color.
  for(i in 1:length(monitors)){
    if(individ.params==TRUE)
      p<-parameters[[i]]
    else
      p<-parameters
    dfm<-DFMClass(monitors[i],p)
    tmp<-CumulativePI.DFM(dfm,ShowPlots = FALSE)
    DFM<-rep(dfm$ID,nrow(tmp))
    tmp<-data.frame(tmp,DFM)
    if(!exists("results",inherits = FALSE))
      results<-tmp
    else
      results<-rbind(results,tmp)
    
  }
  Treatment<-apply(results,1,GetTreatmentForRow,expDesign)
  
  results<-data.frame(results,Treatment)
  results<-subset(results,Treatment!="None")
  
  ## Note that we can just average the curves here because different monitors will have slightly different
  ## values for the minutes column.  They were not all collected at exactly the same time.  So aggregating
  ## won't work.
  p<-ggplot(results, aes(Minutes, PI, group=interaction(DFM,Chamber))) + geom_line(aes(color=Treatment),size=1) +
    geom_smooth(aes(x=Minutes,y = PI,group = Treatment,color=Treatment),size=3)
  show(p)
  
  if(SaveToFile==TRUE){ 
    graphics.off()
  }
  
}
CumulativeEventPIPlots<-function(monitors,parameters,expDesign,events.limit=NA,by.bout=FALSE,SaveToFile=FALSE){
  individ.params<-FALSE
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  if(SaveToFile==TRUE){
    filename<-paste("CumulativeEventPI.pdf",sep="")
    pdf(file=filename)
  }
  
  ## Basically plot the culuative PI plot for each chamber for each treatment
  ## Each Treatment will get a different color.
  for(i in 1:length(monitors)){
    if(individ.params==TRUE)
      p<-parameters[[i]]
    else
      p<-parameters
    dfm<-DFMClass(monitors[i],p)
    tmp<-CumulativeEventPI.DFM(dfm,events.limit,ShowPlots = FALSE)
    DFM<-rep(dfm$ID,nrow(tmp))
    tmp<-data.frame(tmp,DFM)
    if(!exists("results",inherits = FALSE))
      results<-tmp
    else
      results<-rbind(results,tmp)
    
  }
  Treatment<-apply(results,1,GetTreatmentForRow,expDesign)
  
  results<-data.frame(results,Treatment)
  results<-subset(results,Treatment!="None")
  
  ## Note that we have to smooth the curves here because different monitors will have slightly different
  ## values for the minutes column.  They were not all collected at exactly the same time.  So aggregating
  ## won't work.
  if(by.bout==TRUE){
    p<-ggplot(results, aes(EventNum, PI, group=interaction(DFM,Chamber))) + geom_line(aes(color=Treatment),size=1) +
      geom_smooth(aes(x=EventNum,y = PI,group = Treatment,color=Treatment),size=3) + xlab("Event Number")+ ylab("PI (Events)")
  }
  else {
    p<-ggplot(results, aes(Minutes, PI, group=interaction(DFM,Chamber))) + geom_line(aes(color=Treatment),size=1) +
      geom_smooth(aes(x=Minutes,y = PI,group = Treatment,color=Treatment),size=3) + xlab("Minutes") + ylab("PI (Events)")
  }
  show(p)
  
  if(SaveToFile==TRUE){ 
    graphics.off()
  }
}


##### Data output ######
## Current Type options are (case sensitive): BaselinedData, Durations, TimeBtw, TotalFeeding
OutputData.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0),Type="BaselinedData",filename=NA){
  if(Type=="BaselinedData"){
    if(is.na(filename)){
      OutputBaselinedData.Monitors(monitors,parameters,range)
    }
    else{
      OutputBaselinedData.Monitors(monitors,parameters,range,filename)
    }
  }
  else if(Type=="Durations"){
    if(is.na(filename)){
      OutputDurationData.Monitors(monitors,parameters,expDesign,range)
    }
    else{
      OutputDurationData.Monitors(monitors,parameters,expDesign,range,filename)
    }
  }
  else if(Type=="TimeBtw"){
    if(is.na(filename)){
      OutputIntervalData.Monitors(monitors,parameters,expDesign,range) 
    }
    else{
      OutputIntervalData.Monitors(monitors,parameters,expDesign,range,filename) 
    }
  }
  else if(Type=="TotalFeeding"){
    if(is.na(filename)){
      OutputTotalFeeding.Monitors(monitors,parameters,expDesign,range)
    }
    else {
      OutputTotalFeeding.Monitors(monitors,parameters,expDesign,range,filename)
    }
  }
  else {
    stop("Data output type does not exist.")
  }
}

##### Data functions for Exploring Individual DFMs ######
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
  intervals<-cut(y+0.000001,y,include.lowest=TRUE,dig.lab=8)
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
GetIntervalData.DFM<-function(dfm,range=c(0,0)){
  for(i in 1:12){
    tmp<-GetIntervalData.Well(dfm,i,range)
    if(i==1)
      result<-tmp
    else
      result<-rbind(result,tmp)  
  }
  result
}
GetDurationData.DFM<-function(dfm,range=c(0,0)){
  for(i in 1:12){
    tmp<-GetDurationData.Well(dfm,i,range)
    if(i==1)
      result<-tmp
    else
      result<-rbind(result,tmp)  
  }
  result
}

##### Plots for Exploring Individual DFMs ######
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
  if(length(tmp)==1 && tmp[1]==0){
    fake.entry1<-c(FirstSampleData(dfm)$Minutes,0,0,0,0,0,0,0)
    fake.entry2<-c(LastSampleData(dfm)$Minutes,0,0,0,0,0,0,0)
    fake.entry<-data.frame(rbind(fake.entry1,fake.entry2),row.names=c(1,2))
    names(fake.entry)<-c("Minutes","Licks","Duration","TotalIntensity","AvgIntensity","MinIntensity","MaxIntensity","VarIntensity")
    tmp<-fake.entry
    
  }
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
  show(gp)
}


CumulativePI.DFM<-function(dfm, range=c(0,0), ShowPlots=TRUE, SinglePlot=FALSE){
  ## Get the Feeding.PI
  chambers<-1:nrow(dfm$Parameters$Chamber.Sets)
  Minutes<-Minutes(dfm,range)
  for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
    
    if(dfm$Parameters$PI.Multiplier==1){
      wellA<-dfm$Parameters$Chamber.Sets[i,1]
      wellB<-dfm$Parameters$Chamber.Sets[i,2] 
    }
    else {
      wellB<-dfm$Parameters$Chamber.Sets[i,1]
      wellA<-dfm$Parameters$Chamber.Sets[i,2] 
    }
    
    a<-FeedingData.Well.Licks(dfm,wellA,range)
    b<-FeedingData.Well.Licks(dfm,wellB,range)
    FeedingLicksA<-cumsum(a)
    FeedingLicksB<-cumsum(b)
    
    ## Here it is the instantaneous PI
    Feeding.PI<-data.frame(Minutes,(FeedingLicksA - FeedingLicksB)/(FeedingLicksA+FeedingLicksB),FeedingLicksA+FeedingLicksB)
    Feeding.PI<-Feeding.PI[a+b>0,]
    Feeding.PI<-data.frame(Feeding.PI,rep(chambers[i],nrow(Feeding.PI)))
    names(Feeding.PI)<-c("Minutes","PI","Licks","Chamber")
    
    if(i==1)
      results<-Feeding.PI
    else
      results<-rbind(results,Feeding.PI)
    
  }
  if(ShowPlots==TRUE){
    if(SinglePlot==FALSE) 
      gp<-ggplot(results,aes(Minutes,PI,color=Licks)) + geom_line() + facet_grid(rows=vars(factor(Chamber))) +geom_point() +
        ggtitle(paste("DFM",dfm$ID)) + ylab("PI (Licks)") + labs(color="Licks") + ylim(c(-1,1)) + scale_color_gradientn(colours = rainbow(5))
    else
      gp<-ggplot(results,aes(Minutes,PI,color=factor(Chamber))) + geom_line() +geom_point() +
        ggtitle(paste("DFM",dfm$ID)) + ylab("PI (Licks)") + labs(color="Chamber") + ylim(c(-1,1))
    
    show(gp)
  }
  results
}
CumulativeEventPI.DFM<-function(dfm, events.limit=NA, range=c(0,0), ShowPlots=TRUE, SinglePlot=FALSE){
  ## Get the Feeding.PI
  chambers<-1:nrow(dfm$Parameters$Chamber.Sets)
  Minutes<-Minutes(dfm,range)
  for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
    
    if(dfm$Parameters$PI.Multiplier==1){
      wellA<-dfm$Parameters$Chamber.Sets[i,1]
      wellB<-dfm$Parameters$Chamber.Sets[i,2] 
    }
    else {
      wellB<-dfm$Parameters$Chamber.Sets[i,1]
      wellA<-dfm$Parameters$Chamber.Sets[i,2] 
    }
    
    a<-FeedingData.Well.Events(dfm,wellA,range)
    b<-FeedingData.Well.Events(dfm,wellB,range)
    a[a>0]<-1
    b[b>0]<-1
    FeedingEventsA<-cumsum(a)
    FeedingEventsB<-cumsum(b)
    
    ## Here it is the instantaneous PI
    Feeding.PI<-data.frame(Minutes,(FeedingEventsA - FeedingEventsB)/(FeedingEventsA+FeedingEventsB))
    Feeding.PI<-Feeding.PI[a+b>0,]
    if(nrow(Feeding.PI)>0){
      Feeding.PI<-data.frame(Feeding.PI,rep(chambers[i],nrow(Feeding.PI)),1:nrow(Feeding.PI))
      names(Feeding.PI)<-c("Minutes","PI","Chamber","EventNum")
      if(is.na(events.limit)==FALSE && events.limit<nrow(Feeding.PI)){
        Feeding.PI<-Feeding.PI[1:events.limit,]
      }
      if(i==1)
        results<-Feeding.PI
      else
        results<-rbind(results,Feeding.PI)
    }
  }
  if(ShowPlots==TRUE){
    if(SinglePlot==FALSE) 
      gp<-ggplot(results,aes(Minutes,PI,color=EventNum)) + geom_line() + facet_grid(rows=vars(factor(Chamber))) +geom_point() +
        ggtitle(paste("DFM",dfm$ID)) + ylab("PI (Events)") + labs(color="Events") + ylim(c(-1,1))+ scale_color_gradientn(colours = rainbow(5))
    else
      gp<-ggplot(results,aes(Minutes,PI,color=factor(Chamber))) + geom_line() + geom_point() +
        ggtitle(paste("DFM",dfm$ID)) + ylab("PI (Events)") + labs(color="Events") + ylim(c(-1,1))
    show(gp)
  }
  results
}



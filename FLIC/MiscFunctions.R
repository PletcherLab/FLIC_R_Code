require(ggplot2)
require(stats)
require(gridExtra)

## Give filename to this function and it will do analyses of 
## variance with and without DFM as a factor.  It will also plot
## the data for each well for each treatment in each DFM to look for
## consistency.
QuickAOV.FeedingSummary<-function(datafile="FeedingSummary.csv"){
  data<-read.csv(datafile)
  data$Licks<-data$Licks^0.25
  data$DFM<-factor(data$DFM)
  
  aov.1<-aov(Licks~Treatment,data=data)
  print(summary(aov.1))
  cat("\n********\n")
  aov.2<-aov(Licks~DFM+Treatment,data=data)
  print(summary(aov.2))
  
  g1<-ggplot(data,aes(x=DFM,y=Licks)) + geom_boxplot() + geom_jitter(aes(color=Treatment))
  show(g1)
  
  
  g2<-ggplot(data,aes(x=DFM,y=Licks, fill=Treatment)) + geom_boxplot(aes(fill=Treatment))
  show(g2)
}
PlotLicksandLight.Well<-function(dfm,well,range=c(0,0),TransformLicks=TRUE){
  tmp<-FeedingData.Well.Licks(dfm,well)
  SumLicks<-cumsum(tmp)
  if(TransformLicks==TRUE)
    SumLicks<-SumLicks^0.25
  Lights<-GetLightsInfo(dfm)
  Light<-Lights[,paste("W",well,sep="")]
  row<-1
  col<-1
  Row<-rep(row,length(tmp))
  Col<-rep(col,length(tmp))
  Well<-rep(1,length(tmp))
  Size <- Light*2
  Size[Size==0]<-1
  results<-data.frame(dfm$LickData$Minutes,SumLicks,Light,Size,Row,Col,Well)
  names(results)<-c("Minutes","SumLicks","Light","Size","Row","Col","Well")
  if(sum(range)!=0) {
    results<- results[(results$Minutes>range[1]) & (results$Minutes<range[2]),]
  }   
  if(TransformLicks==TRUE)
    ylabel="Transformed Cumulative Licks"
  else
    ylabel="Cumulative Licks"
  gp<-ggplot(results,aes(Minutes,SumLicks,color=Light)) + geom_point(size=results$Size) +
    ggtitle(paste("DFM ",dfm$ID, "; Well ",well, sep=""))+ ylab(ylabel)+ labs(color="Light")
  
  gp
}


GetLightsInfo<-function(dfm){
  Column1<-dfm$RawData$OptoCol1
  Column2<-dfm$RawData$OptoCol2
  data<-data.frame(Column1,Column2)
  data2<-apply(data,1,LightStatus)
  data2<-t(data2)
  final.data<-data.frame(dfm$RawData$Minutes,data2,data)
  names(final.data)<-c("Minutes",paste("W",1:12,sep=""),"OptoCol1","OptoCol2")
  final.data
}
LightStatus<-function(cols){
  col1<-cols[1]
  col2<-cols[2]
  lights<-rep(FALSE,12)
  for(i in 0:5){
    tmp<-bitwShiftL(1,i)
    if(bitwAnd(tmp,col1)>0){
      lights[i*2+1]<-TRUE
    }
    if(bitwAnd(tmp,col2)>0){
      lights[i*2+2]<-TRUE
    }
  }
  lights
}


ScrollEventPlots<-function(dfm,wellnum){
  events<-unique(dfm$BaselineData$Sample)
  currentevent<-1
  keepgoing<-TRUE
  while (keepgoing==TRUE){
    cat("Return = Forward; b = back; q = quit")
    PlotSingleSampleEvent(dfm,wellnum,events[currentevent])
    tmp<-readline()
    if(tmp[1]==""){
      currentevent <- currentevent+1
      if(currentevent>length(events))
        currentevent<-1
    }
    else if(tmp[1]=="b"){
      currentevent<-currentevent-1
      if(currentevent<1)
        currentevent=length(events)
    }
    
    else if(tmp[1]=="q"){
      keepgoing<-FALSE
    }
  }
}


PlotSingleSampleEvent<-function(dfm,wellnum,eventnum){
  tmp<-subset(dfm$BaselineData,dfm$BaselineData$Sample==eventnum)
  w<-paste("W",wellnum,sep="")
  gp<-ggplot(tmp,aes(Minutes,tmp[,(6+wellnum)])) + geom_line(color="red",size=1.2) + facet_wrap(tmp$Sample) +geom_point(color="blue",size=4) +
    ggtitle(paste("DFM: ",dfm$ID,"   Well: ",w,"   Event:",eventnum)) + ylab("Signal") + labs(color="Chamber")
  show(gp)
}



GetCircMin<-function(a){
  
}

# THis accepts the startTime as MM/DD/YYYY HH:MM:SS in military time
# The data frame argument must have a column entitled "Minutes", which
# is assumed to be the elapsed time of the experiment.
AddRealTimeColumns<-function(data,startTimeString){
  dtm<-strptime(startTimeString, format = "%m/%d/%Y %H:%M:%S")
  secs<-data$Minutes*60
  realTimes<-as.POSIXlt(dtm+secs)
  tmp<-names(data)
  cm<-realTimes$hour*60+realTimes$min+realTimes$sec/60
  tmp<-c("RealTime","CircMinutes",tmp)
  r<-data.frame(realTimes,cm,data)
  names(r)<-tmp
  r
}


## Beta Code for examining summary feeding parameters by a fixed number of
## events rather than a fixed time interval.
GetEventRangeInMinutes.Well.OneWell<-function(dfm,well,events=c(0,0)){
  tmp<-FeedingData.Events(dfm)
  cname=paste("W",well,sep="")
  data<-tmp[,cname]  
  min<-tmp$Minutes
  data<-cumsum(data>0)
  if(sum(events)!=0) {
    start<-events[1]
    end<-events[2]
    #print(data[data>=start & data<=end])
    min2<-min[data>=start & data<=end]
  }
  else {
    min2<-min
  }
  if(length(min2)==0){
    result<-c(min[length(min)],min[length(min)])  
  }
  else {
    result<-c(min2[1],min2[length(min2)])
  }
  
  result
}
GetEventRangeInMinutes.Well.TwoWell<-function(dfm,chamber,events=c(0,0)){
  tmp<-FeedingData.Events(dfm)
  if(chamber==1){
    d1<-tmp[,"W1"]
    d2<-tmp[,"W2"]
  }else if(chamber==2){
    d1<-tmp[,"W3"]
    d2<-tmp[,"W4"]
  }else if(chamber==3){
    d1<-tmp[,"W5"]
    d2<-tmp[,"W6"]
  }else if(chamber==4){
    d1<-tmp[,"W7"]
    d2<-tmp[,"W8"]
  }else if(chamber==5){
    d1<-tmp[,"W9"]
    d2<-tmp[,"W10"]
  }else if(chamber==6){
    d1<-tmp[,"W11"]
    d2<-tmp[,"W12"]
  }
  data<-d1+d2
  min<-tmp$Minutes
  data<-cumsum(data>0)
  if(sum(events)!=0) {
    start<-events[1]
    end<-events[2]
    #print(data[data>=start & data<=end])
    min2<-min[data>=start & data<=end]
  }
  else {
    min2<-min
  }
  if(length(min2)==0){
    result<-c(min[length(min)],min[length(min)])  
  }
  else {
    result<-c(min2[1],min2[length(min2)])
  }
  
  result
}
Feeding.Summary.TwoWell.ByEvents<-function(dfm,events=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
    range<-GetEventRangeInMinutes.Well.TwoWell(dfm,i,events)
    # Need a small correction here to avoid missing the first event
    range[1]<-range[1]-0.0034
    
    
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
                   "MeanTimeBtwB","MedTimeBtwB","MeanIntA","MedianIntA","MinIntA","MaxIntA",
                   "MeanIntB","MedianIntB","MinIntB","MaxIntB","StartMin","EndMin")
  if(TransformLicks==TRUE){
    result$LicksA<-result$LicksA^0.25
    result$LicksB<-result$LicksB^0.25
  }
  result    
  
}
Feeding.Summary.OneWell.ByEvents<-function(dfm,events=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for single chambers only")
  
  for(i in 1:12 ){
    range<-GetEventRangeInMinutes.Well.OneWell(dfm,i,events)
    # Need a small correction here to avoid missing the first event
    range[1]<-range[1]-0.0034
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
                   "MeanTimeBtw","MedTimeBtw","MeanInt","MedianInt","MinInt","MaxInt","StartMin","EndMin")
  if(TransformLicks==TRUE)
    result$Licks<-result$Licks^0.25
  result    
}
Feeding.Summary.DFM.ByEvents<-function(dfm,events=c(0,0),TransformLicks=TRUE){
  if(dfm$Parameters$Chamber.Size==1)
    Feeding.Summary.OneWell.ByEvents(dfm,events,TransformLicks)
  else if(dfm$Parameters$Chamber.Size==2)
    Feeding.Summary.TwoWell.ByEvents(dfm,events,TransformLicks)
  else
    stop("Feeding Summary not implemented for this DFM type.")    
}

Feeding.Summary.DFM.BySingleEvents<-function(dfm,events=c(1,2),TransformLicks=TRUE){
  events<-events[1]:events[2]
  for(i in 1:(length(events)-1)){
    new.events<-c(events[i],events[i]+0.5)
    if(dfm$Parameters$Chamber.Size==1)
      tmp<-Feeding.Summary.OneWell.ByEvents(dfm,new.events,TransformLicks)
    else if(dfm$Parameters$Chamber.Size==2)
      tmp<-Feeding.Summary.TwoWell.ByEvents(dfm,new.events,TransformLicks)
    else
      stop("Feeding Summary not implemented for this DFM type.")    
    EventNum<-rep(events[i],nrow(tmp))
    tmp<-data.frame(EventNum,tmp)
    if(i==1){
      results<-tmp
    }
    else {
      results<-rbind(results,tmp)
    }
  }
  results
}

Feeding.Summary.Monitors.BySingleEvents<-function(monitors,parameters,events=c(1,2),expDesign=NA,SaveToFile=TRUE,TransformLicks=TRUE,filename="FeedingSummaryBySingleEvents"){
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
    tmp<-Feeding.Summary.DFM.BySingleEvents(dfm,events,TransformLicks)      
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


BinnedFeeding.Summary.DFM.ByEvents<-function(dfm,binsize.Enum,TransformLicks=TRUE){
  tmp<-Feeding.TotalEvents(dfm)
  y<-seq(0,max(tmp),by=binsize.Enum)
  
  tmpMatrix<-cbind(y[-length(y)],y[-1])
  tmpMatrix[,1]<-tmpMatrix[,1]+1
  intervals<-cut(y+0.000001,y,include.lowest=TRUE,dig.lab=8)
  intervals<-intervals[-length(intervals)]
  result<-Feeding.Summary.DFM.ByEvents(dfm,events=tmpMatrix[1,],TransformLicks)
  Interval<-rep(intervals[1],nrow(result))
  Minutes<-rep(mean(tmpMatrix[1,]),nrow(result))
  result<-data.frame(Interval,Minutes,result)
  for(i in 2:nrow(tmpMatrix)){
    tmp<-Feeding.Summary.DFM.ByEvents(dfm,events=tmpMatrix[i,],TransformLicks)
    Interval<-rep(intervals[i],nrow(tmp))
    Minutes<-rep(mean(tmpMatrix[i,]),nrow(tmp))
    tmp<-data.frame(Interval,Minutes,tmp)
    result<-rbind(result,tmp)
  }
  result
}
BinnedFeeding.Summary.Monitors.ByEvents<-function(monitors,parameters,binsize.Enum=5,expDesign=NA,SaveToFile=TRUE,TransformLicks=TRUE,filename="BinnedSummaryByEvents"){
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
    tmp<-BinnedFeeding.Summary.DFM.ByEvents(dfm,binsize.Enum,TransformLicks)      
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


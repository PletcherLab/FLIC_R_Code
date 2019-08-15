## Private Class Methods for FLIC Class V2
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)


#####Treatment based functions######
Feeding.BinnedSummary.Monitors<-function(monitors,parameters,binsize.min=30,expDesign=NA,range=c(0,0),SaveToFile=TRUE,TransformLicks=TRUE){
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
    tmp<-BinLickData(dfm,binsize.min,range,TransformLicks)      
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
      filename<-paste("BinnedLicks_TRT_Stats",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(trt.summary,file=filename,row.names=FALSE)
      filename<-paste("BinnedLicks_TRT_Data",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(results,file=filename,row.names=FALSE)
    }
  }
  else if(SaveToFile==TRUE){
    filename<-paste("BinnedLicks_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
  }
  
  if(is.data.frame(expDesign)) {
    return(list(Results=results,Stats=trt.summary))
  }
  else {
    return(list(Results=results))
  }
}
Feeding.BinnedEvents.Monitors<-function(monitors,parameters,binsize.min=30,expDesign=NA,range=c(0,0),SaveToFile=TRUE){
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
    tmp<-BinEventData(dfm,binsize.min,range)      
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
      filename<-paste("BinnedEvents_TRT_Stats",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(trt.summary,file=filename,row.names=FALSE)
      filename<-paste("BinnedEvents_TRT_Data",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(results,file=filename,row.names=FALSE)
    }
  }
  else if(SaveToFile==TRUE){
    filename<-paste("BinnedEvents_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
  }
  
  if(is.data.frame(expDesign)) {
    return(list(Results=results,Stats=trt.summary))
  }
  else {
    return(list(Results=results))
  }
}
Feeding.BinnedLicksPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingLicks.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    FeedingLicks.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}


FeedingBinnedLicks.OneWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE,TransformLicks=TRUE){
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























#####Binning functions######

OutputBinnedEvents.Monitors<-function(monitors,parameters,binsize.min,range=c(0,0)){
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
    ##print(paste("Outputting Interval Data for DFM ",j,".",sep=""))
    ##flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    pnames<-Get.Parameter.Names(p)
    parameter.vector<-GetParameterVector(p)
    tmp<-BinFeedingData.Events(dfm,binsize.min,range)
    
    DFM<-rep(monitor,nrow(tmp))
    tmp<-data.frame(DFM,tmp)
    tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
    tmp3<-cbind(tmp,tmp2)
    names(tmp3)<-c(names(tmp),pnames)
    
    if(j==1){
      result<-tmp3
    }
    else {
      result<-rbind(result,tmp3)
    }
  }
  
  filename<-paste("BinnedEvents_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}
BinLickData<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE){
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
  names(result)<-c("Interval","Min","Licks","Events","DFM","Chamber")
  ## Note that transformation occurs after the summation.
  if(TransformLicks==TRUE)
    result$Licks<-result$Licks^0.25
  result  
}
BinEventData<-function(dfm,binsize.min,range=c(0,0)){
  result<-BinFeedingData.Well.Events(dfm,1,binsize.min,range)
  Well<-factor(rep(1,nrow(result)))
  DFM<-factor(rep(dfm$ID,nrow(result)))
  result<-data.frame(result,DFM,Well)
  
  for(i in 2:12) {
    tmp<-BinFeedingData.Well.Events(dfm,i,binsize.min,range)
    Well<-factor(rep(i,nrow(tmp)))
    DFM<-factor(rep(dfm$ID,nrow(tmp)))
    tmp<-data.frame(tmp,DFM,Well)
    result<-rbind(result,tmp)
  }
  names(result)<-c("Interval","Min","Events","DFM","Chamber")
  result  
}
GetCumLicksPlots.DFM<-function(dfm,FacetPlots=TRUE,TransformLicks=TRUE){
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
  if(TransformLicks==TRUE)
    ylabel="Transformed Cumulative Licks"
  else
    ylabel="Cumulative Licks"
  if(FacetPlots==TRUE) {
  gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line() + facet_grid(rows=vars(Row),cols=vars(Col)) +geom_point() +
    ggtitle(paste("DFM",dfm$ID)) + ylab(ylabel)
  }
  else {
  gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line(size=1.2) +
    ggtitle(paste("DFM",dfm$ID))+ ylab(ylabel)
  }
  gp
}
GetTotalLicksPlot.Monitors<-function(monitors, p, range=c(0,0),TransformLicks=TRUE){
  tmp2<-Feeding.Summary.Monitors(monitors,p,range=range,TransformLicks)
  tmp2<-tmp2$Results
  ylabel="Licks"
  if(TransformLicks==TRUE) {
    ylabel="Transformed Licks"
  }
  tmp2$DFM<-factor(tmp2$DFM)
  gp<-ggplot(tmp2,aes(x=DFM,y=Licks,fill=DFM)) + geom_dotplot(binaxis='y',stackdir='center',stackratio=1.5, dotsize=0.7) +
    ylab(ylabel)
  gp
}
PlotBins.Licks<-function(dfm,binsize.min,range=c(0,0),TransformLicks=TRUE){
  binnedData<-BinLickData(dfm,binsize.min,range,TransformLicks)
  ylabel<-"Licks"
  xlabel<-"Minutes"
  ttl<-paste("DFM:",dfm$ID)
  if(TransformLicks==TRUE) {
    ylabel<-"Transformed Licks"
    ttl<-paste("DFM:",dfm$ID," (Transformed)")
  }
  gp<-ggplot(binnedData,aes(x=Min,y=Licks,fill=Well)) + geom_bar(stat="identity") + facet_grid(Well ~ .) + ggtitle(paste("DFM:",dfm$ID)) +
    theme(legend.position = "none") + ylab(ylabel) + xlab(xlabel)
  show(gp)
}


#####Private functions######










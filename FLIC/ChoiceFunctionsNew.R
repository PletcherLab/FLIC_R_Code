source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)
require(ggplot2)

Feeding.FinalPIPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  individ.params<-FALSE
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  if(SaveToFile==TRUE){
    filename<-paste("FinalPIBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      if(individ.params==TRUE)
        p<-parameters[[1]]
      else
        p<-parameters
      dfm<-DFMClass(monitors[1],p)
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
    ## Tmp Fix
    sumLicks<-results$LicksA+results$LicksB
    results<-results[sumLicks>0,]
    
    r<-paste("PI -- Range: (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$PI)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(aes(color = LicksA+LicksB),size=3,height=0) +
            ylim(c(-1.05,1.05)) + ggtitle(r) + xlab("Treatment") +ylab("PI") + guides(fill=FALSE))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("PI -- Range: (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$PI)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(aes(color = LicksA+LicksB),size=3,height=0) +
                    ylim(c(-1.05,1.05)) + ggtitle(r) + xlab("Treatment") +ylab("PI") + guides(fill=FALSE))
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
Feeding.FinalEventPIPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  individ.params<-FALSE
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  if(SaveToFile==TRUE){
    filename<-paste("FinalEventPIBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      if(individ.params==TRUE)
        p<-parameters[[1]]
      else
        p<-parameters
      dfm<-DFMClass(monitors[1],p)
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
    r<-paste("EventPI -- Range: (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(Treatment, EventPI)) + geom_boxplot(aes(fill = Treatment),outlier.size=-1) + geom_jitter(aes(color = EventsA+EventsB),size=3,height=0) +
            ylim(c(-1.05,1.05)) + ggtitle(r) + xlab("Treatment") +ylab("PI") + guides(fill=FALSE))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      {
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("EventPI -- Range: (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<-(ggplot(results, aes(Treatment, EventPI)) + geom_boxplot(aes(fill = Treatment),outlier.size=-1) + geom_jitter(aes(color = EventsA+EventsB),size=3,height=0) +
                   ylim(c(-1.05,1.05)) + ggtitle(r) + xlab("Treatment") +ylab("PI") + guides(fill=FALSE))
      }
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
    if(SaveToFile==TRUE)
      graphics.off()
  }
} 
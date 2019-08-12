## Private Class Methods for FLIC Class V2.1
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)
require(ggplot2)


#####Treatment based functions######
## Treatment based functions
## Divisions will create a separate graph for the cumulative PI (starting at range[0]) up
## to each of the division points in the experiment.  This is distinct from the time-dependent
## PI because it will always start from the first point in the data set (satisfying the first
## range parameter).
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
    
    r<-paste("PI -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$PI)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(-1.05,1.05)) + ggtitle(r) + xlab("Treatment") +ylab("PI") + guides(fill=FALSE))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("PI -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$PI)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
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
Feeding.PIPlots.Trt<-function(monitors,parameters,expDesign,range=c(0,0),SaveToFile=FALSE){
  individ.params<-FALSE
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  if(SaveToFile==TRUE){
    filename<-paste("PIs_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
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
    for(j in 1:6){
      p<-PIData.Chamber(dfm,j,range)
      n<-cumsum(p$Feeding.PI)
      d<-cumsum(abs(p$Feeding.PI))
      y<-n/d
      y[d==0]<-0
      tmp<-rep(GetTreatmentForChamber(monitors[i],j,expDesign),length(y))
      tmp.m<-rep(monitors[i],length(y))
      tmp.c<-rep(j,length(y))
      tmp<-data.frame(p$Minutes,y,tmp,tmp.m,tmp.c)
      names(tmp)<-c("Minutes","PI","Treatment","DFM","Chamber")
      if(!exists("results",inherits = FALSE))
        results<-tmp
      else
        results<-rbind(results,tmp)
    }
  }
  
  results<-subset(results,Treatment!="None")
  
  ## Note that we can just average the curves here because different monitors will have slightly different
  ## values for the minutes column.  They were not all collected at exactly the same time.  So aggregating
  ## won't work.
  p<-ggplot(results, aes(Minutes, PI, group=interaction(DFM,Chamber))) + geom_line(aes(color=Treatment),size=1) +
    geom_smooth(aes(x=Minutes,y = PI,group = Treatment,color=Treatment),size=3)
  show(p)
  if(SaveToFile==TRUE){ 
    filename<-paste("PIs_TRT",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
    graphics.off()
  }
  else 
    return(results)
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
    r<-paste("EventPI -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$EventPI)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(-1.05,1.05)) + ggtitle(r) + xlab("Treatment") +ylab("PI") + guides(fill=FALSE))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("EventPI -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<-(ggplot(results$Results, aes(results$Treatment, results$EventPI)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
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
    if(SaveToFile==TRUE)
      graphics.off()
  }
} 
Feeding.EventPIPlots.Trt<-function(monitors,parameters,expDesign,range=c(0,0),SaveToFile=FALSE){
  individ.params<-FALSE
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  if(SaveToFile==TRUE){
    filename<-paste("EventPIs_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
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
    for(j in 1:6){
      p<-PIData.Chamber(dfm,j,range)
      n<-cumsum(p$Feeding.EventPI)
      d<-cumsum(abs(p$Feeding.EventPI))
      y<-n/d
      y[d==0]<-0
      tmp<-rep(GetTreatmentForChamber(monitors[i],j,expDesign),length(y))
      tmp.m<-rep(monitors[i],length(y))
      tmp.c<-rep(j,length(y))
      tmp<-data.frame(p$Minutes,y,tmp,tmp.m,tmp.c)
      names(tmp)<-c("Minutes","EventPI","Treatment","DFM","Chamber")
      if(!exists("results",inherits = FALSE))
        results<-tmp
      else
        results<-rbind(results,tmp)
    }
  }
  results<-subset(results,Treatment!="None")
  ## Note that we can just average the curves here because different monitors will have slightly different
  ## values for the minutes column.  They were not all collected at exactly the same time.  So aggregating
  ## won't work.
  p<-ggplot(results, aes(Minutes, EventPI, group=interaction(DFM,Chamber))) + geom_line(aes(color=Treatment),size=1) +
    geom_smooth(aes(x=Minutes,y = EventPI,group = Treatment,color=Treatment),size=3)
  show(p)
  if(SaveToFile==TRUE){ 
    filename<-paste("EventPIs_TRT",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
    graphics.off()
  }
  else 
    return(results)
}
Feeding.TimeDependentPIPlots.Trt<-function(monitors,parameters,expDesign,window.size.min=30,step.size.min=3,symbol.mult=4,range=c(0,0),SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("TimeDepPIs_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  for(i in 1:length(monitors)){
    dfm<-DFMClass(monitors[i],parameters)
    for(j in 1:6){
      tmp<-Feeding.GetTimeDependentPIData(dfm,j,window.size.min,step.size.min,symbol.mult,range)     
      tmp2<-rep(GetTreatmentForChamber(monitors[i],j,expDesign),nrow(tmp))
      tmp.m<-rep(monitors[i],nrow(tmp))
      tmp.c<-rep(j,nrow(tmp))
      n2<-names(tmp)
        tmp<-data.frame(tmp2,tmp.m,tmp.c,tmp)
      names(tmp)<-c("Treatment","DFM","Chamber",n2)
      if(!exists("results",inherits = FALSE))
        results<-tmp
      else
        results<-rbind(results,tmp)
    }
  }
  
  results<-subset(results,Treatment!="None")
  
  p1<-ggplot(results, aes(Minutes, Feeding.PI, group=interaction(DFM,Chamber))) + geom_line(aes(color=Treatment),size=1) +
    geom_smooth(aes(x=Minutes,y = Feeding.PI,group = Treatment,color=Treatment),size=3) + ggtitle("Time-Dependent PI") + xlab("Minutes") +ylab("PI") 
  p2<-ggplot(results, aes(Minutes, Feeding.EventPI, group=interaction(DFM,Chamber))) + geom_line(aes(color=Treatment),size=1) +
    geom_smooth(aes(x=Minutes,y = Feeding.EventPI,group = Treatment,color=Treatment),size=3)+ ggtitle("Time-Dependent Event PI") + xlab("Minutes") +ylab("Event PI") 
  multiplot(p1,p2,cols=1)
  if(SaveToFile==TRUE){ 
    filename<-paste("TimeDepPIs_TRT",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
    graphics.off()
  }
  else 
    return(results)
} 
Feeding.BoutPI.Trt<-function(monitors,parameters,minEvents,expDesign, range=c(0,0),SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("BoutPIs_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  for(i in 1:length(monitors)){
    dfm<-DFMClass(monitors[i],parameters)
    for(j in 1:6){
      tmp<-Feeding.GetBoutPIData(dfm,j,minEvents,range)     
      tmp2<-rep(GetTreatmentForChamber(monitors[i],j,expDesign),nrow(tmp))
      tmp.m<-rep(monitors[i],nrow(tmp))
      tmp.c<-rep(j,nrow(tmp))
      n2<-names(tmp)
      tmp<-data.frame(tmp2,tmp.m,tmp.c,tmp)
      names(tmp)<-c("Treatment","DFM","Chamber",n2)
      if(!exists("results",inherits = FALSE))
        results<-tmp
      else
        results<-rbind(results,tmp)
    }
  }
  
  results<-subset(results,Treatment!="None")
  
  p1<-ggplot(results, aes(CumEvents, Feeding.PI, group=interaction(DFM,Chamber), weight=TotalEvents)) + 
    geom_jitter(aes(color=Treatment,size=TotalEvents),height=0) + 
    geom_smooth(aes(x=CumEvents,y = Feeding.PI,group = Treatment,color=Treatment),size=2) +
    ggtitle("Bout PI") + xlab("Feeding Bout Number") +ylab("PI")
  
  p2<-ggplot(results, aes(CumEvents, Feeding.EventPI, group=interaction(DFM,Chamber), weight=TotalEvents)) + 
    geom_jitter(aes(color=Treatment,size=TotalEvents),height=0) + 
    geom_smooth(aes(x=CumEvents,y = Feeding.EventPI,group = Treatment,color=Treatment),size=2) +
    ggtitle("Bout Event PI") + xlab("Feeding Bout Number") +ylab("Event PI")
  
  multiplot(p1,p2,cols=1)
  if(SaveToFile==TRUE){ 
    filename<-paste("TimeDepPIs_TRT",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
    graphics.off()
  }
  else 
    return(results)
}

#####Chamber based functions######
## Chamber based functions 
PIData.Chamber<-function(dfm,chamber,range=c(0,0)){
  cname=paste("C",chamber,sep="")
  tmp<-dfm$PIData[[cname]]
  if(sum(range)!=0) {
    tmp<-tmp[tmp$Minutes>range[1] & tmp$Minutes<range[2],] 
  }
  tmp
}
Feeding.FinalPI.Chamber<-function(dfm,chamber,range=c(0,0)) {
  tmp<-PIData.Chamber(dfm,chamber,range)
  n<-sum(tmp$Feeding.PI)
  d<-sum(abs(tmp$Feeding.PI))    
  if(d==0)
    result<-0
  else
    result<-n/d  
  result
}
Feeding.FinalEventPI.Chamber<-function(dfm,chamber,range=c(0,0)) {
  tmp<-PIData.Chamber(dfm,chamber,range)
  n<-sum(tmp$Feeding.EventPI)
  d<-sum(abs(tmp$Feeding.EventPI))    
  if(d==0)
    result<-0
  else
    result<-n/d  
  result
}
Feeding.CumulativePIData<-function(dfm,chamber,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  d<-PIData.Chamber(dfm,chamber,range)
  
  n<-cumsum(d$Feeding.PI)
  den<-cumsum(abs(d$Feeding.PI))
  y<-n/den
  y[den==0]<-0
  
  Feeding.PI<-y
  CumLicksA<-cumsum(FeedingData.Well.Licks(dfm,wellA,range))
  CumLicksB<-cumsum(FeedingData.Well.Licks(dfm,wellB,range))
  
  n<-cumsum(d$Feeding.EventPI)
  den<-cumsum(abs(d$Feeding.EventPI))
  y<-n/den
  y[den==0]<-0
  
  Feeding.EventPI<-y
  AEvents<-FeedingData.Well.Events(dfm,wellA,range)
  BEvents<-FeedingData.Well.Events(dfm,wellB,range)
  CumEventsA<-cumsum(AEvents>0)
  CumEventsB<-cumsum(BEvents>0)
  
  result<-data.frame(d$Minutes,Feeding.PI,CumLicksA,CumLicksB,Feeding.EventPI,
                     CumEventsA,CumEventsB)
  names(result)<-c("Minutes","Feeding.PI","CumLicksA","CumLicksB",
                   "Feeding.EventPI","CumEventsA","CumEventsB")
  result
}
Tasting.CumulativePIData<-function(dfm,chamber,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  d<-PIData.Chamber(dfm,chamber,range)
  
  n<-cumsum(d$Tasting.PI)
  den<-cumsum(abs(d$Tasting.PI))
  y<-n/den
  y[den==0]<-0
  
  Tasting.PI<-y
  CumLicksA<-cumsum(TastingData.Well(dfm,wellA,range))
  CumLicksB<-cumsum(TastingData.Well(dfm,wellB,range))
  
  result<-data.frame(d$Minutes,Tasting.PI,CumLicksA,CumLicksB)
  names(result)<-c("Minutes","Tasting.PI","CumLicksA","CumLicksB")                   
  result
}

## Produces one plot
## Shows raw data for each of the two channels
RawDataPlot.TwoWell<-function(dfm,chamber,range=c(0,0)){ 
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")  
  rd<-RawData(dfm,range)
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  dA<-rd[,nameA]
  dB<-rd[,nameB]
  ymax<-max(c(dA,dB))
  ymin<-min(c(dA,dB))
  plot(rd$Minutes,dA,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(rd$Minutes,dB,col=2)  
  title(paste("DFM:",dfm$ID," C:",chamber,"- Raw Data"))
}

## Produces one plot
## Shows data after a baseline correction has been applied
BaselineDataPlot.TwoWell<-function(dfm,chamber,range=c(0,0)){ 
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  rd<-BaselinedData(dfm,range)
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  dA<-rd[,nameA]
  dB<-rd[,nameB]
  ymax<-max(c(dA,dB))
  ymin<-min(c(dA,dB))
  plot(rd$Minutes,dA,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(rd$Minutes,dB,col=2)  
  title(paste("DFM:",dfm$ID," C:",chamber,"- Baselined Data"))
}

## Produces two plots
## Shows baseline corrected data for each Plate
## together with the current threshold superimposed.
Feeding.ThresholdPlots.TwoWell<-function(dfm,chamber,range=c(0,0)){ 
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  rd<-BaselinedData(dfm)
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  dA<-rd[,nameA]
  dB<-rd[,nameB]
  
  tA<-dfm$Thresholds[[nameA]]
  tB<-dfm$Thresholds[[nameB]]
  
  x<-rd$Minutes
  
  if(sum(range)!=0) {
    tA<-tA[rd$Minutes>range[1] & rd$Minutes<range[2],]
    tB<-tB[rd$Minutes>range[1] & rd$Minutes<range[2],]
    dA<-dA[rd$Minutes>range[1] & rd$Minutes<range[2]]
    dB<-dB[rd$Minutes>range[1] & rd$Minutes<range[2]]
    x<-x[x>range[1] & x<range[2]]
  }
  
  newData<-data.frame(x,dA,tA$FeedingMin,tA$FeedingMax)
  names(newData)<-c("x","a","ta","tb")
  
  
  
  ymax<-max(c(newData$a))
  ymin<-min(c(newData$a))
  plot(newData$x,newData$a,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(newData$x,newData$ta,col=2,lty=2)    
  lines(newData$x,newData$tb,col=2,lty=2)  
  title(paste("DFM:",dfm$ID," C:",chamber,"A",sep=""))
  
  
  newData<-data.frame(x,dB,tB$FeedingMin,tB$FeedingMax)
  names(newData)<-c("x","b","ta","tb")
  
  if(sum(range)!=0) {
    newData<- newData[(newData$x>range[1]) & (newData$x<range[2]),]
  }
  
  ymax<-max(c(newData$b))
  ymin<-min(c(newData$b))
  plot(newData$x,newData$b,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(newData$x,newData$tb,col=2,lty=2)  
  lines(newData$x,newData$ta,col=2,lty=2)  
  title(paste("DFM:",dfm$ID," C:",chamber,"B",sep=""))
}
Tasting.ThresholdPlots<-function(dfm,chamber,range=c(0,0)){ 
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  rd<-BaselinedData(dfm)
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  dA<-rd[,nameA]
  dB<-rd[,nameB]
  
  tA<-dfm$Thresholds[[nameA]]
  tB<-dfm$Thresholds[[nameB]]
  x<-rd$Minutes
  if(sum(range)!=0) {
    tA<-tA[rd$Minutes>range[1] & rd$Minutes<range[2],]
    tB<-tB[rd$Minutes>range[1] & rd$Minutes<range[2],]
    dA<-dA[rd$Minutes>range[1] & rd$Minutes<range[2]]
    dB<-dB[rd$Minutes>range[1] & rd$Minutes<range[2]]
    x<-x[x>range[1] & x<range[2]]
  }
  
  newData<-data.frame(x,dA,tA$TastingMin,tA$TastingMax)
  names(newData)<-c("x","a","ta","tb")
  
  ymax<-max(c(newData$a))
  ymin<-min(c(newData$a))
  plot(newData$x,newData$a,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(newData$x,newData$ta,col=2,lty=2)    
  lines(newData$x,newData$tb,col=2,lty=2)  
  title(paste("DFM:",dfm$ID," C:",chamber,"Threshold Plate A"))
  
  
  newData<-data.frame(x,dB,tB$TastingMin,tB$TastingMax)
  names(newData)<-c("x","b","ta","tb")
  
  if(sum(range)!=0) {
    newData<- newData[(newData$x>range[1]) & (newData$x<range[2]),]
  }
  
  ymax<-max(c(newData$b))
  ymin<-min(c(newData$b))
  plot(newData$x,newData$b,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(newData$x,newData$tb,col=2,lty=2)  
  lines(newData$x,newData$ta,col=2,lty=2)  
  title(paste("DFM:",dfm$ID," C:",chamber,"Threshold Plate B"))
}

## Produces two plots
## Shows cumulative licks/tastes/events and (second plot) cumulative PI
Feeding.PIPlots.Chamber<-function(dfm,chamber,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  tmp<-FeedingData.Licks(dfm,range)
  FeedingLicksA<-tmp[,nameA]
  FeedingLicksB<-tmp[,nameB]
  ymax<-max(Feeding.TotalLicks.Well(dfm,wellA,range),Feeding.TotalLicks.Well(dfm,wellB,range))
  plot(tmp$Minutes,cumsum(FeedingLicksA),type="l",ylab="Licks",xlab="Minutes",ylim=c(0,ymax))
  lines(tmp$Minutes,cumsum(FeedingLicksB),col=2)
  title(paste("DFM:",dfm$ID," C:",chamber,"Licks"))
  
  p<-PIData.Chamber(dfm,chamber,range)
  n<-cumsum(p$Feeding.PI)
  d<-cumsum(abs(p$Feeding.PI))
  y<-n/d
  y[d==0]<-0
  
  plot(p$Minutes,y,type="l",ylab="PI",xlab="Minutes",ylim=c(-1,1),lwd=2)
  title(paste("DFM:",dfm$ID," C:",chamber,"Feeding PI"))
}
Tasting.PIPlots<-function(dfm,chamber,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  tmp<-TastingData(dfm,range)
  FeedingLicksA<-tmp[,nameA]
  FeedingLicksB<-tmp[,nameB]
  ymax<-max(Tasting.TotalLicks.Well(dfm,wellA,range),Tasting.TotalLicks.Well(dfm,wellB,range))
  plot(tmp$Minutes,cumsum(FeedingLicksA),type="l",ylab="Licks",xlab="Minutes",ylim=c(0,ymax))
  lines(tmp$Minutes,cumsum(FeedingLicksB),col=2)
  title(paste("DFM:",dfm$ID," C:",chamber,"Tastes"))
  
  p<-PIData.Chamber(dfm,chamber,range)
  n<-cumsum(p$Tasting.PI)
  d<-cumsum(abs(p$Tasting.PI))
  y<-n/d
  y[d==0]<-0
  
  plot(p$Minutes,y,type="l",ylab="PI",xlab="Minutes",ylim=c(-1,1),lwd=2)
  title(paste("DFM:",dfm$ID," C:",chamber,"Tasting PI"))
}
Feeding.EventPIPlots<-function(dfm,chamber,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  tmp<-FeedingData.Events(dfm,range)
  FeedingLicksA<-tmp[,nameA]
  ## The event data returned has the duration encoded, so remove that.
  FeedingLicksA[FeedingLicksA>0]<-1
  FeedingLicksB<-tmp[,nameB]
  FeedingLicksB[FeedingLicksB>0]<-1
  ymax<-max(Feeding.TotalEvents.Well(dfm,wellA,range),Feeding.TotalEvents.Well(dfm,wellB,range))
  plot(tmp$Minutes,cumsum(FeedingLicksA),type="l",ylab="Licks",xlab="Minutes",ylim=c(0,ymax))
  lines(tmp$Minutes,cumsum(FeedingLicksB),col=2)
  title(paste("DFM:",dfm$ID," C:",chamber,"Events"))
  
  p<-PIData.Chamber(dfm,chamber,range)
  n<-cumsum(p$Feeding.EventPI)
  d<-cumsum(abs(p$Feeding.EventPI))
  y<-n/d
  y[d==0]<-0
  
  plot(p$Minutes,y,type="l",ylab="PI",xlab="Minutes",ylim=c(-1,1),lwd=2)
  title(paste("DFM:",dfm$ID," C:",chamber,"Event PI"))
}

Feeding.GetTimeDependentPIData<-function(dfm,chamber,window.size.min=30,step.size.min=3,symbol.mult=4,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")

  half.window<-window.size.min/2
  if(range[1]>FirstSampleData(dfm)$Minutes)
    low<-range[1]
  else
    low<-FirstSampleData(dfm)$Minutes
  low<-low + half.window
  
  if(range[2]!=0 && range[2]<LastSampleData(dfm)$Minutes)
    high<-range[2]
  else
    high<-LastSampleData(dfm)$Minutes
  
  ## Get latest minute possible
  high<-high-half.window
  
  tmp<-seq(low,high,by=step.size.min)
  results<-data.frame(matrix(rep(-99,(length(tmp)*7)),ncol=7))
  names(results)<-c("Minutes","Feeding.PI","CumLicksA","CumLicksB","Feeding.EventPI",
                    "CumEventsA","CumEventsB")
  
  for(i in 1:length(tmp)){
    results[i,1]<-tmp[i]
    
    pp<-Feeding.CumulativePIData(dfm,chamber,c(tmp[i]-half.window,tmp[i]+half.window))        
    ii<-nrow(pp)
    
    results[i,3]<-pp$CumLicksA[ii]
    results[i,4]<-pp$CumLicksB[ii]
    results[i,2]<-pp$Feeding.PI[ii]
    
    results[i,6]<-pp$CumEventsA[ii]
    results[i,7]<-pp$CumEventsB[ii]
    results[i,5]<-pp$Feeding.EventPI[ii]    
  }  
  results
}
Feeding.GetBoutPIData<-function(dfm,chamber,minEvents, range=c(0,0)){
  tmp<-Feeding.CumulativePIData(dfm,chamber,range)
  tmp2<-(tmp$CumEventsA+tmp$CumEventsB)
  tmp3<-ceiling((tmp2)/(minEvents))
  tmp4<-rle(tmp3)  
  tmp5<-tmp$Minutes[cumsum(tmp4$lengths)]
  tmp<-c(tmp$Minutes[1],tmp5)
  
  num.periods<-length(tmp)-1
  results<-data.frame(matrix(rep(-99,(num.periods*8)),ncol=8))
  names(results)<-c("MinStart","MinEnd","Feeding.PI","CumLicksA","CumLicksB","Feeding.EventPI","CumEventsA","CumEventsB")
  
  for(i in 1:(length(tmp)-1)){
    results[i,1]<-tmp[i]
    results[i,2]<-tmp[i+1]
    
    pp<-Feeding.CumulativePIData(dfm,chamber,c(tmp[i],tmp[i+1]))    
    ii<-nrow(pp)
    results[i,4]<-pp$CumLicksA[ii]
    results[i,5]<-pp$CumLicksB[ii]
    results[i,3]<-pp$Feeding.PI[ii]
    results[i,8]<-pp$CumEventsA[ii]
    results[i,7]<-pp$CumEventsB[ii]
    results[i,6]<-pp$Feeding.EventPI[ii]
  }
  TotalEvents<-results$CumEventsA+results$CumEventsB
  CumEvents<-cumsum(TotalEvents)
  results<-data.frame(results,TotalEvents,CumEvents)
  
  ## Need to remove the first entry cause that lists the tiem until the first feeding
  results[-1,]  
}


## Produces two plots, one for PI and one for Event PI with the relevant PIs calculated
## only within a moving window.
Feeding.TimeDependentPIPlots.Chamber<-function(dfm,chamber,window.size.min=30,step.size.min=3,symbol.mult=4, range=c(0,0)){
  results<-Feeding.GetTimeDependentPIData(dfm,chamber,window.size.min,step.size.min,symbol.mult,range)
 
  plot(results[,1],results[,2],ylab="PI",xlab="Minutes",ylim=c(-1,1),type="l",lwd=2)
  tmp<-results[,3]+results[,4]
  max.tmp<-max(tmp)
  min.tmp<-min(tmp)
  tmp<-tmp/max.tmp*symbol.mult  
  points(results[,1],results[,2],pch=21,bg=3,cex=tmp)
  title(paste("DFM:",dfm$ID," C:",chamber,"Time-Dep Feeding PI"))
  
  yval=0.9
  if(results[1,2]>0.5) yval<-(-0.55)
  txt<-paste("Max Licks:",max.tmp)
  text(results[1,1],yval,txt,adj=c(0,0),cex=0.8)
  txt<-paste("Min Licks:",min.tmp)
  text(results[1,1],yval-0.15,txt,adj=c(0,0),cex=0.8)
  
  plot(results[,1],results[,5],ylab="PI",xlab="Minutes",ylim=c(-1,1),type="l",lwd=2)
  tmp<-results[,6]+results[,7]
  max.tmp<-max(tmp)
  min.tmp<-min(tmp)
  tmp<-tmp/max.tmp*symbol.mult
  points(results[,1],results[,5],pch=21,bg=4,cex=tmp)
  title(paste("DFM:",dfm$ID," C:",chamber,"Time-Dep Event PI"))
  
  yval=0.9
  if(results[1,5]>0.5) yval<-(-0.55)
  txt<-paste("Max Events:",max.tmp)
  text(results[1,1],yval,txt,cex=0.8,adj=c(0,0))
  txt<-paste("Min Events:",min.tmp)
  text(results[1,1],yval-0.15,txt,adj=c(0,0),cex=0.8)
  
  results  
}

## Segmented PI plots break the data into segments where a defined number
## of drinking events have occurred (set by minEvents). PI and EventPI are then calculated 
## within each interval and plotted.
## This function calculates a standard PI for each segment of the data
## where segments are defined by # Events = minEvents.
Feeding.BoutPIPlot.Chamber<-function(dfm,chamber,minEvents,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  if(range[1]>FirstSampleData(dfm)$Minutes)
    xlim.low<-range[1]
  else
    xlim.low<-FirstSampleData(dfm)$Minutes
  
  if(range[2]!=0 && range[2]<LastSampleData(dfm)$Minutes)
    xlim.high<-range[2]
  else
    xlim.high<-LastSampleData(dfm)$Minutes
  
  results<-Feeding.GetBoutPIData(dfm,chamber,minEvents,range)
  num.periods<-nrow(results)
  n<-xlim.low:xlim.high
  
  plot(n,n,ylab="PI",xlab="Minutes",xlim=c(xlim.low,xlim.high),ylim=c(-1,1),type="n")
  title(paste("DFM:",dfm$ID," C:",chamber,"Bout PI"))
  if(num.periods>0){
    yval<-rep(-99,num.periods)
    for(i in 1:(nrow(results))){
      pp<-Feeding.CumulativePIData(dfm,chamber,c(results[i,1],results[i,2]))    
      ii<-nrow(pp)
      y<-pp$Feeding.PI
      if(max(y)<.8) yval[i]=.60
      else yval[i]=(-1.0)
      lines(pp$Minutes,y,col=i,lwd=2) 
    }
    positions<-rep(3,length(results[,3]))
    #positions[results[,3]>0.5]<-1  
    x<-(results[,1]+results[,2])/2
    vals<-results$CumLicksA+results$CumLicksB
    text(x,yval,as.character(round(vals,2)),pos=positions,offset=1)
  }
  else {
    abline(h=0,lwd=2) 
    text(mean(n),-0.5,paste("No Feeding Data"))
  }
  results  
}

## This function calculates an Event PI for each segment of the data
## where segments are defined by # Events = minEvents.
Feeding.BoutEventPIPlot.Chamber<-function(dfm,chamber,minEvents,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  if(range[1]>FirstSampleData(dfm)$Minutes)
    xlim.low<-range[1]
  else
    xlim.low<-FirstSampleData(dfm)$Minutes
  
  if(range[2]!=0 && range[2]<LastSampleData(dfm)$Minutes)
    xlim.high<-range[2]
  else
    xlim.high<-LastSampleData(dfm)$Minutes
  
  results<-Feeding.GetBoutPIData(dfm,chamber,minEvents,range)
  
  num.periods<-nrow(results)
  n<-xlim.low:xlim.high
  
  plot(n,n,ylab="PI",xlab="Minutes",xlim=c(xlim.low,xlim.high),ylim=c(-1,1),type="n")
  title(paste("DFM:",dfm$ID," C:",chamber,"Bout EventPI"))
  yval<-rep(-99,num.periods)
  if(num.periods>0){
    for(i in 1:(nrow(results))){
      pp<-Feeding.CumulativePIData(dfm,chamber,c(results[i,1],results[i,2]))    
      ii<-nrow(pp)
      y<-pp$Feeding.EventPI
      if(max(y)<.8) yval[i]=.60
      else yval[i]=(-1.0)
      lines(pp$Minutes,y,col=i,lwd=2) 
    }
    positions<-rep(3,length(results[,3]))
    #positions[results[,3]>0.5]<-1  
    x<-(results[,1]+results[,2])/2
    vals<-results$CumEventsA+results$CumEventsB
    text(x,yval,as.character(round(vals,2)),pos=positions,offset=1)
  }
  else {
    abline(h=0,lwd=2) 
    text(mean(n),-0.5,paste("No Feeding Data"))
  }
  results  
}

## This function will output the cumulative (i.e., running) PI
## value and Event PI data (along with minutes, parameter values, etc) to a 
## .csv file.
Feeding.OutputRawPIData.Chamber<-function(dfm,chamber,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  parameter.vector<-GetDFMParameterVector(dfm)
  pnames<-Get.Parameter.Names(dfm$Parameters)
  tmp<-Feeding.CumulativePIData(dfm,chamber,range)
  
  tmp<-data.frame(cbind(rep(dfm$ID,nrow(tmp)),rep(chamber,nrow(tmp)),tmp))
  tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
  
  tmp3<-cbind(tmp,tmp2)
  final.names<-c("DFM","Chamber",names(tmp)[c(-1,-2)],pnames)
  names(tmp3)<-final.names
  
  filename<-paste("Feeding_RawPIData_DFM",dfm$ID,"_","C",chamber,".csv",sep="") 
  write.csv(tmp3,file=filename,row.names=FALSE)
}
Tasting.OutputRawPIData.Chamber<-function(dfm,chamber,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  parameter.vector<-GetDFMParameterVector(dfm)
  pnames<-Get.Parameter.Names(dfm$Parameters)
  tmp<-Tasting.CumulativePIData(dfm,chamber,range)
  
  tmp<-data.frame(cbind(rep(dfm$ID,nrow(tmp)),rep(chamber,nrow(tmp)),tmp))
  tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
  
  tmp3<-cbind(tmp,tmp2)
  final.names<-c("DFM","Chamber",names(tmp)[c(-1,-2)],pnames)
  names(tmp3)<-final.names
  
  filename<-paste("Tasting_RawPIData_DFM",dfm$ID,"_","C",chamber,".csv",sep="") 
  write.csv(tmp3,file=filename,row.names=FALSE)
}
#####Monitor based functions######
## Multiple Monitor based functions 

## Will output a pdf file showing rawdata, baseline corrected data,
## threshold plots, and PI/EventPI curves for each of the six
## chambers in each listed monitor
## used by Jenny
First.Pass.Analysis<-function(monitors,parameters){  
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
    outfile<-paste("DFM_",monitor,".pdf",sep="")    
    pdf(file=outfile)
    par(mfrow=c(3,2))
    dfm<-DFMClass(monitor,p)  
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")
    for(i in 1:6){      
      RawDataPlot.TwoWell(dfm,i)
      BaselineDataPlot.TwoWell(dfm,i)        
    }
    graphics.off()
  }
}
TimeDependentPI.Monitors<-function(monitors,parameters,expDesign=NA,window.size.min=30,step.size.min=3,symbol.mult=4,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  if(individ.params==TRUE)
    p<-parameters[[1]]
  else
    p<-parameters
  pnames<-Get.Parameter.Names(p)
  filename<-paste("TimeDepPIs_DFM",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
  
  pdf(file=filename)
  par(mfrow=c(2,2))
  
  for(j in 1:length(monitors)){
    print(paste("Analyzing Time Dependent PI for DFM ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    x<-1:6
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    parameter.vector<-GetParameterVector(p)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    for(i in x){  
      tmp<-Feeding.TimeDependentPIPlots.Chamber(dfm,i,window.size.min,step.size.min,symbol.mult=symbol.mult,range)
      tmp<-data.frame(cbind(rep(monitor,nrow(tmp)),rep(i,nrow(tmp)),1:nrow(tmp),tmp))
      tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
      tmp3<-cbind(tmp,tmp2)
      if(i==1 && j==1){
        results<-tmp3
        names(results)<-c("DFM","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
      }
      else {
        names(tmp3)<-c("DFM","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
        results<-rbind(results,tmp3)  
      }
      
    }        
  }
  graphics.off();
  
  if(is.data.frame(expDesign))
    results<-AppendTreatmentonResultsFrame(results,expDesign)
  
  filename<-paste("TimeDepPIResults_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
  write.csv(results,file=filename,row.names=FALSE)
}

## Will output Segmented PI plot for each chamber in each monitor and
## save the Segemented PI data in a file.
BoutPI.Monitors<-function(monitors,parameters,minEvents,expDesign=NA,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  filename<-paste("BoutPIs_DFM",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
  
  pdf(file=filename)
  par(mfrow=c(3,2))
  
  for(j in 1:length(monitors)){
    print(paste("Analyzing BoutPI for DFM ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    x<-1:6
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    pnames<-Get.Parameter.Names(p)
    parameter.vector<-GetParameterVector(p)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    for(i in x){    
      tmp<-Feeding.BoutPIPlot.Chamber(dfm,i,minEvents,range)
      num.periods<-nrow(tmp)
      if(num.periods==0)
        next
      tmp<-data.frame(cbind(rep(monitor,nrow(tmp)),rep(i,nrow(tmp)),1:nrow(tmp),tmp))
      tmp2<-matrix(rep(parameter.vector,num.periods),ncol=length(parameter.vector),byrow=TRUE)
      
      tmp3<-cbind(tmp,tmp2)
      if(i==1 && j==1){
        results<-tmp3
        names(results)<-c("DFM","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
      }
      else {
        names(tmp3)<-c("DFM","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
        results<-rbind(results,tmp3)  
      }
      
    }        
  }
  
  graphics.off();
  
  if(is.data.frame(expDesign))
    results<-AppendTreatmentonResultsFrame(results,expDesign)
  
  filename<-paste("BoutPIResults_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
  write.csv(results,file=filename,row.names=FALSE)
}

## Same as above except focussing on the EventPI
BoutEventPI.Monitors<-function(monitors,parameters,minEvents,expDesign=NA,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  
  filename<-paste("BoutEventPIs_M",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
  
  pdf(file=filename)
  par(mfrow=c(3,2))
  
  for(j in 1:length(monitors)){
    print(paste("Analyzing Bout EventPI for DFM ",monitors[j],".",sep=""))
    flush.console() 
    monitor<-monitors[j]
    x<-1:6
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    pnames<-Get.Parameter.Names(p)
    parameter.vector<-GetParameterVector(p)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    for(i in x){   
      tmp<-Feeding.BoutEventPIPlot.Chamber(dfm,i,minEvents,range)
      num.periods<-nrow(tmp)
      if(num.periods==0)
        next
      tmp<-data.frame(cbind(rep(monitor,nrow(tmp)),rep(i,nrow(tmp)),1:nrow(tmp),tmp))
      tmp2<-matrix(rep(parameter.vector,num.periods),ncol=length(parameter.vector),byrow=TRUE)
      
      tmp3<-cbind(tmp,tmp2)
      if(i==1 && j==1){
        results<-tmp3
        names(results)<-c("DFM","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
      }
      else {
        names(tmp3)<-c("DFM","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
        results<-rbind(results,tmp3)  
      }
      
    }        
  }
  graphics.off();
  
  if(is.data.frame(expDesign))
    results<-AppendTreatmentonResultsFrame(results,expDesign)
  
  filename<-paste("BoutEventPIResults_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
  write.csv(results,file=filename,row.names=FALSE)
}

## This function will output the cumulative (i.e., running) PI
## values and EventPI values (along with minutes, parameter values, etc) to a 
## separate .csv file for each chamber in each of the specified monitors.
Feeding.OutputRawPIData.Monitors<-function(monitors,parameters,range=c(0,0)){
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
    print(paste("Outputting feeding PI Data for DFM ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    x<-1:6
    for(i in x){      
      Feeding.OutputRawPIData.Chamber(dfm,i,range)
    }
  }
}
Tasting.OutputRawPIData.Monitors<-function(monitors,parameters,range=c(0,0)){
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
    print(paste("Outputting tasting PI Data for DFM ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    x<-1:6
    for(i in x){      
      Tasting.OutputRawPIData.Chamber(dfm,i,range)
    }
  }
}
####DFM Based functions#######
## These functions make plots for all chambers in a DFM
## Mostly used for the GUI wrapper.
Feeding.PIPlots.DFM<-function(dfm,range=c(0,0)){
  windows(record=FALSE,width=16,height=12) # opens a window and starts recording
  op <- par(ask=FALSE)
  on.exit(par(op))
  par(mfrow=c(3,4))
  for(i in 1:6){
    Feeding.PIPlots.Chamber(dfm,i,range)    
  }
  windows.options(record=FALSE) #stops recording.  
}
Feeding.EventPIPlots.DFM<-function(dfm,range=c(0,0)){
  windows(record=FALSE,width=16,height=12) # opens a window and starts recording
  op <- par(ask=FALSE)
  on.exit(par(op))
  par(mfrow=c(3,4))
  for(i in 1:6){
    Feeding.EventPIPlots(dfm,i,range)    
  }
  windows.options(record=FALSE) #stops recording.  
}
Feeding.TimeDependentPIPlots.DFM<-function(dfm,window.size.min=30,step.size.min=3,symbol.mult=4,range=c(0,0)){
  windows(record=FALSE,width=16,height=12) # opens a window and starts recording
  op <- par(ask=FALSE)
  on.exit(par(op))
  par(mfrow=c(3,4))
  for(i in 1:6){
    Feeding.TimeDependentPIPlots.Chamber(dfm,i,window.size.min,step.size.min,symbol.mult,range)    
  }
  windows.options(record=FALSE) #stops recording.  
}
Feeding.BoutPIPlot.DFM<-function(dfm,minEvents,range=c(0,0)) {
  windows(record=FALSE,width=8,height=12) # opens a window and starts recording
  op <- par(ask=FALSE)
  on.exit(par(op))
  par(mfrow=c(3,2))
  for(i in 1:6){
    Feeding.BoutPIPlot.Chamber(dfm,i,minEvents,range)    
  }
  windows.options(record=FALSE) #stops recording.
}
Feeding.BoutEventPIPlot.DFM<-function(dfm,minEvents,range=c(0,0)) {
  windows(record=FALSE,width=8,height=12) # opens a window and starts recording
  op <- par(ask=FALSE)
  on.exit(par(op))
  par(mfrow=c(3,2))
  for(i in 1:6){
    Feeding.BoutEventPIPlot.Chamber(dfm,i,minEvents,range)    
  }
  windows.options(record=FALSE) #stops recording.
}


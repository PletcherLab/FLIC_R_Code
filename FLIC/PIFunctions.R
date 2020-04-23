
Feeding.CumulativePIPlots.Trt<-function(monitors,parameters,expDesign,range=c(0,0),SaveToFile=FALSE){
  individ.params<-FALSE
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  if(SaveToFile==TRUE){
    filename<-paste("PI_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
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
    tmp<-Feeding.CumulativePI.DFM(dfm,ShowPlots = FALSE)
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
  return(results)
  if(SaveToFile==TRUE){ 
    filename<-paste("PI_TRT",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
    graphics.off()
  }
  else 
    return(results)
}
Feeding.CumulativeEventPIPlots.Trt<-function(monitors,parameters,expDesign,events.limit=NA,by.bout=FALSE,SaveToFile=FALSE){
  individ.params<-FALSE
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  if(SaveToFile==TRUE){
    filename<-paste("EventPI_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
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
    tmp<-Feeding.CumulativeEventPI.DFM(dfm,events.limit,ShowPlots = FALSE)
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
  return(results)
  if(SaveToFile==TRUE){ 
    filename<-paste("EventPI_TRT",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
    graphics.off()
  }
  else 
    return(results)
}

BinnedFeeding.PIPlot.Trt<-function(monitors,parameters,binsize.min=30,expDesign=NA,range=c(0,0),SaveToFile=FALSE){
  mystderr <- function(x, na.rm=FALSE) {
    if (na.rm) x <- na.omit(x)
    sqrt(var(x)/length(x))
  }
  tmp<-BinnedFeeding.Summary.Monitors(monitors,parameters,binsize.min,expDesign,range, SaveToFile=FALSE, TransformLicks = FALSE)
  licksA<-tmp$Results$LicksA
  licksB<-tmp$Results$LicksB
  licksPI1<-(licksA-licksB)/(licksA+licksB)
  tmp2<-data.frame(tmp$Results$Treatment,tmp$Results$Interval,tmp$Results$Min,licksPI1,licksA+licksB)
  names(tmp2)<-c("Treatment","Interval","Min","PI","Licks")
  
  tmp3.mean<-aggregate(tmp2$PI,by=list(tmp2$Min,tmp2$Treatment),mean,na.rm=TRUE)
  tmp3.sem<-aggregate(tmp2$PI,by=list(tmp2$Min,tmp2$Treatment),mystderr,na.rm=TRUE)
  tmp3.licks<-aggregate(tmp2$Licks,by=list(tmp2$Min,tmp2$Treatment),mean,na.rm=TRUE)
  
  tmp3<-data.frame(tmp3.mean[,1:3],tmp3.sem[,3],tmp3.licks[,3])
  names(tmp3)<-c("Min","Treatment","Mean","SEM","Licks")
  
  pd <- position_dodge(5) # move them .05 to the left and right
  gp<-ggplot(tmp3,aes(x=Min,y=Mean,color=Treatment,group=Treatment)) + 
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM,color=Treatment), width=.1, position=pd) +
    geom_line(position=pd,size=1) +
    geom_point(position=pd, size=4, shape=21, fill="White") +xlab("Minutes") + ylab("PI") + ylim(c(-1,1)) 
  if(SaveToFile==TRUE){
    filename<-paste("BinnedFeedingPI_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    ggsave(filename,gp)
  }
  show(gp)
}
BinnedFeeding.EventPIPlot.Trt<-function(monitors,parameters,binsize.min=30,expDesign=NA,range=c(0,0),SaveToFile=FALSE){
  mystderr <- function(x, na.rm=FALSE) {
    if (na.rm) x <- na.omit(x)
    sqrt(var(x)/length(x))
  }
  tmp<-BinnedFeeding.Summary.Monitors(monitors,parameters,binsize.min,expDesign,range, SaveToFile=FALSE, TransformLicks = FALSE)
  licksA<-tmp$Results$EventsA
  licksB<-tmp$Results$EventsB
  licksPI1<-(licksA-licksB)/(licksA+licksB)
  tmp2<-data.frame(tmp$Results$Treatment,tmp$Results$Interval,tmp$Results$Min,licksPI1)
  names(tmp2)<-c("Treatment","Interval","Min","EventPI")
  
  tmp3.mean<-aggregate(tmp2$EventPI,by=list(tmp2$Min,tmp2$Treatment),mean,na.rm=TRUE)
  tmp3.sem<-aggregate(tmp2$EventPI,by=list(tmp2$Min,tmp2$Treatment),mystderr,na.rm=TRUE)
  
  tmp3<-data.frame(tmp3.mean[,1:3],tmp3.sem[,3])
  names(tmp3)<-c("Min","Treatment","Mean","SEM")
  
  pd <- position_dodge(5) # move them .05 to the left and right
  gp<-ggplot(tmp3,aes(x=Min,y=Mean,color=Treatment,group=Treatment)) + 
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM,color=Treatment), width=.1, position=pd) +
    geom_line(position=pd,size=1) +
    geom_point(position=pd, size=4, shape=21, fill="white") +xlab("Minutes") + ylab("PI (Events)") + ylim(c(-1,1))
  if(SaveToFile==TRUE){
    filename<-paste("BinnedFeedingEventPI_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    ggsave(filename,gp)
  }
  show(gp)
}

Feeding.CumulativePI.DFM<-function(dfm, range=c(0,0), ShowPlots=TRUE, SinglePlot=FALSE){
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
Feeding.CumulativeEventPI.DFM<-function(dfm, events.limit=NA, range=c(0,0), ShowPlots=TRUE, SinglePlot=FALSE){
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



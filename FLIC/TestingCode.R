rm(list=ls())
source("CommonChamber.R")
source("DFM.R")
source("MiscFunctions.R")

## Testing the Two Well Data with Example
expDesign<-read.csv("ExpDesign.csv")
p.choice.one<-ParametersClass.TwoWell()
p.choice.two<-ParametersClass.TwoWell()
p.choice.two<-SetParameter(p.choice.two,PI.Multiplier=-1.0)
monitors.choice<-c(11,12,13,14,15,16,17)
p.choice.list<-list(p.choice.two,p.choice.one,p.choice.two,p.choice.one,p.choice.two,p.choice.one,p.choice.two)

BinnedFeeding.PIPlots.Trt<-function(monitors,parameters,binsize.min=30,expDesign=NA,range=c(0,0),SaveToFile=TRUE,TransformLicks=TRUE){
  mystderr <- function(x, na.rm=FALSE) {
    if (na.rm) x <- na.omit(x)
    sqrt(var(x)/length(x))
  }
  tmp<-BinnedFeeding.Summary.Monitors(monitors,parameters,binsize.min,expDesign,range, SaveToFile=FALSE, TransformLicks = FALSE)
  licksA<-tmp$Results$LicksA
  licksB<-tmp$Results$LicksB
  licksPI1<-(licksA-licksB)/(licksA+licksB)
  tmp2<-data.frame(tmp$Results$Treatment,tmp$Results$Interval,tmp$Results$Min,licksPI1)
  names(tmp2)<-c("Treatment","Interval","Min","PI")

  tmp3.mean<-aggregate(tmp2$PI,by=list(tmp2$Min,tmp2$Treatment),mean,na.rm=TRUE)
  tmp3.sem<-aggregate(tmp2$PI,by=list(tmp2$Min,tmp2$Treatment),mystderr,na.rm=TRUE)

  tmp3<-data.frame(tmp3.mean[,1:3],tmp3.sem[,3])
  names(tmp3)<-c("Min","Treatment","Mean","SEM")

  pd <- position_dodge(5) # move them .05 to the left and right
  gp<-ggplot(tmp3,aes(x=Min,y=Mean,color=Treatment,group=Treatment)) + 
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM,color=Treatment), width=.1, position=pd) +
    geom_line(position=pd,size=1) +
    geom_point(position=pd, size=4, shape=21, fill="white") +xlab("Minutes") + ylab("PI") + ylim(c(-1,1))
  if(SaveToFile==TRUE){
    filename<-paste("BinnedFeedingPI_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    ggsave(filename,gp)
  }
  show(gp)
}












tmp<-Feeding.EventPIPlots.Trt(monitors.choice,p.choice.list,expDesign)

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


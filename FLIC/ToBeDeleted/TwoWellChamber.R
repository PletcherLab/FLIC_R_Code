## Private Class Methods for FLIC Class V2.1
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)

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
RawDataPlot<-function(dfm,chamber,range=c(0,0)){ 
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
BaselineDataPlot<-function(dfm,chamber,range=c(0,0)){ 
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
Feeding.ThresholdPlots<-function(dfm,chamber,range=c(0,0)){ 
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
  title(paste("DFM:",dfm$ID," C:",chamber,"Threshold Plate A"))
  
  
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
  title(paste("DFM:",dfm$ID," C:",chamber,"Threshold Plate B"))
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

## Produces two plots, one for PI and one for Event PI with the relevant PIs calculated
## only within a moving window.
Feeding.TimeDependentPIPlots<-function(dfm,chamber,window.size.min=30,step.size.min=3,symbol.mult=4){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber,2] 
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  
  ## Get earliest minute possible to avoid edge effects
  low<-floor(FirstSampleData(dfm)$Minutes)+window.size.min
  ## Get latest minute possible
  high<-floor(LastSampleData(dfm)$Minutes)
  tmp<-seq(low,high,by=step.size.min)
  results<-data.frame(matrix(rep(-99,(length(tmp)*7)),ncol=7))
  names(results)<-c("Minutes","Feeding.PI","CumLicksA","CumLicksB","Feeding.EventPI",
                    "CumEventsA","CumEventsB")
  
  for(i in 1:length(tmp)){
    results[i,1]<-tmp[i]
    
    pp<-Feeding.CumulativePIData(dfm,chamber,c(tmp[i]-window.size.min,tmp[i]))        
    ii<-nrow(pp)
    
    results[i,3]<-pp$CumLicksA[ii]
    results[i,4]<-pp$CumLicksB[ii]
    results[i,2]<-pp$Feeding.PI[ii]
    
    results[i,6]<-pp$CumEventsA[ii]
    results[i,7]<-pp$CumEventsB[ii]
    results[i,5]<-pp$Feeding.EventPI[ii]    
  }  
  plot(results[,1],results[,2],ylab="PI",xlab="Minutes",xlim=c(FirstSampleData(dfm)$Minutes,LastSampleData(dfm)$Minutes),ylim=c(-1,1),type="l",lwd=2)
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
  
  plot(results[,1],results[,5],ylab="PI",xlab="Minutes",xlim=c(FirstSampleData(dfm)$Minutes,LastSampleData(dfm)$Minutes),ylim=c(-1,1),type="l",lwd=2)
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
Feeding.BoutPIPlot<-function(dfm,chamber,minEvents){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
 
  tmp<-Feeding.CumulativePIData(dfm,chamber)
  tmp2<-(tmp$CumEventsA+tmp$CumEventsB)
  tmp3<-floor((tmp2)/(minEvents))
  tmp4<-rle(tmp3)  
  tmp5<-tmp$Minutes[cumsum(tmp4$lengths)]
  tmp<-c(tmp$Minutes[1],tmp5)
  
  num.periods<-length(tmp)-1
  results<-data.frame(matrix(rep(-99,(num.periods*5)),ncol=5))
  names(results)<-c("MinStart","MinEnd","Feeding.PI","CumLicksA","CumLicksB")
  n<-1:LastSampleData(dfm)$Minutes
  
  plot(n,n,ylab="PI",xlab="Minutes",xlim=c(0,LastSampleData(dfm)$Minutes),ylim=c(-1,1),type="n")
  title(paste("DFM:",dfm$ID," C:",chamber,"Seg PI"))
  yval<-rep(-99,num.periods)
  for(i in 1:(length(tmp)-1)){
    results[i,1]<-tmp[i]
    results[i,2]<-tmp[i+1]
    
    pp<-Feeding.CumulativePIData(dfm,chamber,c(tmp[i],tmp[i+1]))    
    ii<-nrow(pp)
    results[i,4]<-pp$CumLicksA[ii]
    results[i,5]<-pp$CumLicksB[ii]
    results[i,3]<-pp$Feeding.PI[ii]
    
    y<-pp$Feeding.PI
    if(max(y)<.8) yval[i]=.60
    else yval[i]=(-1.0)
    lines(pp$Minutes,y,col=i,lwd=2) 
  }
  positions<-rep(3,length(results[,3]))
  #positions[results[,3]>0.5]<-1  
  x<-(tmp[-1]+tmp[-length(tmp)])/2
  vals<-results$CumLicksA+results$CumLicksB
  text(x,yval,as.character(round(vals,2)),pos=positions,offset=1)
  
  results  
}
## This function calculates an Event PI for each segment of the data
## where segments are defined by # Events = minEvents.
Feeding.BoutEventPIPlot<-function(dfm,chamber,minEvents){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  tmp<-Feeding.CumulativePIData(dfm,chamber)
  tmp2<-(tmp$CumEventsA+tmp$CumEventsB)
  tmp3<-floor((tmp2)/(minEvents))
  tmp4<-rle(tmp3)  
  tmp5<-tmp$Minutes[cumsum(tmp4$lengths)]
  tmp<-c(tmp$Minutes[1],tmp5)
  
  num.periods<-length(tmp)-1
  results<-data.frame(matrix(rep(-99,(num.periods*5)),ncol=5))
  names(results)<-c("MinStart","MinEnd","Feeding.EventPI","CumEventsA","CumEventsB")
  n<-1:LastSampleData(dfm)$Minutes
  
  plot(n,n,ylab="EventPI",xlab="Minutes",xlim=c(0,LastSampleData(dfm)$Minutes),ylim=c(-1,1),type="n")
  title(paste("DFM:",dfm$ID," C:",chamber,"Seg EventPI"))
  yval<-rep(-99,num.periods)
  for(i in 1:(length(tmp)-1)){
    results[i,1]<-tmp[i]
    results[i,2]<-tmp[i+1]
    
    pp<-Feeding.CumulativePIData(dfm,chamber,c(tmp[i],tmp[i+1]))    
    ii<-nrow(pp)
    results[i,4]<-pp$CumEventsA[ii]
    results[i,5]<-pp$CumEventsB[ii]
    results[i,3]<-pp$Feeding.EventPI[ii]
    
    y<-pp$Feeding.EventPI
    if(max(y)<.8) yval[i]=.60
    else yval[i]=(-1.0)
    lines(pp$Minutes,y,col=i,lwd=2) 
  }
  positions<-rep(3,length(results[,3]))
  #positions[results[,3]>0.5]<-1  
  x<-(tmp[-1]+tmp[-length(tmp)])/2
  vals<-results$CumEventsA+results$CumEventsB
  text(x,yval,as.character(round(vals,2)),pos=positions,offset=1)
  
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


## Will output a pdf file showing rawdata, baseline corrected data,
## threshold plots, and PI/EventPI curves for each of the six
## chambers in each listed monitor
## used by Jenny
First.Pass.Analysis<-function(monitors,parameters){  
  for(j in monitors){
    monitor<-j      
    outfile<-paste("DFM_",monitor,".pdf",sep="")    
    pdf(file=outfile)
    par(mfrow=c(3,2))
    dfm<-DFMClass(monitor,parameters)  
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")
    for(i in 1:6){      
      RawDataPlot(dfm,i)
      BaselineDataPlot(dfm,i)        
    }
    graphics.off()
  }
}
TimeDependentPI.Monitors<-function(monitors,parameters,window.size.min=30,step.size.min=3){
  pnames<-Get.Parameter.Names(parameters)
  parameter.vector<-GetParameterVector(parameters)
  filename<-paste("MovingPIs_DFM",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
  
  pdf(file=filename)
  par(mfrow=c(2,2))
  
  for(j in monitors){
    print(paste("Analyzing MovingPI for DFM ",j,".",sep=""))
    flush.console()
    monitor<-j
    x<-1:6
    dfm<-DFMClass(monitor,parameters)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    for(i in x){  
      tmp<-Feeding.TimeDependentPIPlots(dfm,i,window.size.min,step.size.min,symbol.mult=3)
      
      tmp<-data.frame(cbind(rep(monitor,nrow(tmp)),rep(i,nrow(tmp)),1:nrow(tmp),tmp))
      tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
      
      tmp3<-cbind(tmp,tmp2)
      if(i==1 && j==monitors[1]){
        results<-tmp3
        names(results)<-c("Monitor","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
      }
      else {
        names(tmp3)<-c("Monitor","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
        results<-rbind(results,tmp3)  
      }
      
    }        
  }
  graphics.off();
  filename<-paste("MovingPIResults_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
  write.csv(results,file=filename,row.names=FALSE)
}


## Will output Segmented PI plot for each chamber in each monitor and
## save the Segemented PI data in a file.
BoutPI.Monitors<-function(monitors,parameters,minEvents){
  pnames<-Get.Parameter.Names(parameters)
  parameter.vector<-GetParameterVector(parameters)
  filename<-paste("SegmentedPIs_DFM",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
  
  pdf(file=filename)
  par(mfrow=c(3,2))
  
  for(j in monitors){
    print(paste("Analyzing SegmentedPI for DFM ",j,".",sep=""))
    flush.console()
    monitor<-j
    x<-1:6
    dfm<-DFMClass(monitor,parameters)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    for(i in x){     
      tmp<-Feeding.BoutPIPlot(dfm,i,minEvents)
      num.periods<-nrow(tmp)
      tmp<-data.frame(cbind(rep(monitor,nrow(tmp)),rep(i,nrow(tmp)),1:nrow(tmp),tmp))
      tmp2<-matrix(rep(parameter.vector,num.periods),ncol=length(parameter.vector),byrow=TRUE)
      
      tmp3<-cbind(tmp,tmp2)
      if(i==1 && j==monitors[1]){
        results<-tmp3
        names(results)<-c("Monitor","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
      }
      else {
        names(tmp3)<-c("Monitor","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
        results<-rbind(results,tmp3)  
      }
      
    }        
  }
  graphics.off();
  filename<-paste("SegmentedPIResults_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
  write.csv(results,file=filename,row.names=FALSE)
}

## Same as above except focussing on the EventPI
BoutEventPI.Monitors<-function(monitors,parameters,minEvents){
  pnames<-Get.Parameter.Names(parameters)
  parameter.vector<-GetParameterVector(parameters)
  filename<-paste("SegmentedEventPIs_M",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
  
  pdf(file=filename)
  par(mfrow=c(3,2))
  
  for(j in monitors){
    print(paste("Analyzing Segmented EventPI for DFM ",j,".",sep=""))
    flush.console()
    monitor<-monitors[j]
    x<-1:6
    dfm<-DFMClass(monitor,parameters)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    for(i in x){   
      tmp<-Feeding.BoutEventPIPlot(dfm,i,minEvents)
      num.periods<-nrow(tmp)
      tmp<-data.frame(cbind(rep(monitor,nrow(tmp)),rep(i,nrow(tmp)),1:nrow(tmp),tmp))
      tmp2<-matrix(rep(parameter.vector,num.periods),ncol=length(parameter.vector),byrow=TRUE)
      
      tmp3<-cbind(tmp,tmp2)
      if(i==1 && j==monitors[1]){
        results<-tmp3
        names(results)<-c("Monitor","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
      }
      else {
        names(tmp3)<-c("Monitor","Chamber","Interval",names(tmp)[c(-1,-2,-3)],pnames)
        results<-rbind(results,tmp3)  
      }
      
    }        
  }
  graphics.off();
  filename<-paste("SegmentedEventPIResults_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
  write.csv(results,file=filename,row.names=FALSE)
}

## This function will output the cumulative (i.e., running) PI
## values and EventPI values (along with minutes, parameter values, etc) to a 
## separate .csv file for each chamber in each of the specified monitors.
Feeding.OutputRawPIData.Monitors<-function(monitors,parameters,range=c(0,0)){
  for(j in monitors){
    print(paste("Outputting feeding PI Data for DFM ",j,".",sep=""))
    flush.console()
    monitor<-j
    dfm<-DFMClass(monitor,parameters)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    x<-1:6
    for(i in x){      
      Feeding.OutputRawPIData.Chamber(dfm,i,range)
    }
  }
}
Tasting.OutputRawPIData.Monitors<-function(monitors,parameters,range=c(0,0)){
  for(j in monitors){
    print(paste("Outputting tasting PI Data for DFM ",j,".",sep=""))
    flush.console()
    monitor<-j
    dfm<-DFMClass(monitor,parameters)
    if(dfm$Parameters$Chamber.Size!=2)
      stop("This function is for two-chamber DFM only")    
    x<-1:6
    for(i in x){      
      Tasting.OutputRawPIData.Chamber(dfm,i,range)
    }
  }
}

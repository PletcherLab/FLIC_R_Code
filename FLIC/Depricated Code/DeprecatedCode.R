## Deprecated functions kept for future source cost mining.

OutputBinnedLicks.Monitors<-function(monitors,parameters,binsize.min,range=c(0,0),TransformLicks=TRUE){
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
    tmp<-BinLickData(dfm,binsize.min,range,TransformLicks)
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
  
  filename<-paste("BinnedFeeding_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}

## PI Functions
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

PIData.Chamber<-function(dfm,chamber,range=c(0,0)){
  cname=paste("C",chamber,sep="")
  tmp<-dfm$PIData[[cname]]
  if(sum(range)!=0) {
    tmp<-tmp[tmp$Minutes>range[1] & tmp$Minutes<range[2],] 
  }
  tmp
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
Set.PI.Data<-function(dfm){
  ## Get the Feeding.PI
  cnames<-paste("C",1:nrow(dfm$Parameters$Chamber.Sets),sep="")
  Minutes<-dfm$BaselineData$Minutes
  for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
    
    ## Conflicts are defined as both pads with signal greater than the
    ## minimum value of all feeding and tasting thresholds
    wellA<-dfm$Parameters$Chamber.Sets[i,1]
    wellB<-dfm$Parameters$Chamber.Sets[i,2]
    
    FeedingLicksA<-FeedingData.Well.Licks(dfm,wellA)
    FeedingLicksB<-FeedingData.Well.Licks(dfm,wellB)
    
    ## Here it is the instantaneous PI
    Feeding.PI<-FeedingLicksA - FeedingLicksB  
    
    ## Temporarily eliminate duration information for EventPI.
    tmpA<-FeedingData.Well.Events(dfm,wellA)
    tmpA[tmpA>0]<-1
    tmpB<-FeedingData.Well.Events(dfm,wellB)
    tmpB[tmpB>0]<-1
    Feeding.EventPI<-tmpA-tmpB
    
    TastingLicksA<-TastingData.Well(dfm,wellA)
    TastingLicksB<-TastingData.Well(dfm,wellB)
    
    ## Here it is the instantaneous PI
    Tasting.PI<-TastingLicksA - TastingLicksB  
    
    results<-data.frame(Minutes,Feeding.PI,Feeding.EventPI,Tasting.PI)
    names(results)<-c("Minutes","Feeding.PI", "Feeding.EventPI","Tasting.PI")
    
    if(dfm$Parameters$PI.Multiplier!=1){
      results$Feeding.PI<-results$Feeding.PI*dfm$Parameters$PI.Multiplier
      results$Feeding.EventPI<-results$Feeding.EventPI*dfm$Parameters$PI.Multiplier
      results$Tasting.PI<-results$Tasting.PI*dfm$Parameters$PI.Multiplier
    }
    
    if(i==1){    
      PIData=list(C1=results)      
    }
    else {
      s<-paste("C",i,sep="")      
      PIData[[s]]<-results    
    }        
  }
  row.names(PIData)<-NULL
  dfm$PIData<-PIData
  dfm
}
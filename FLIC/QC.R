## These are functions for quality control.


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


CheckForSimultaneousFeeding.DFM<-function(dfm){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-well chambers only")
  lick.matrix<-matrix(rep(NA,24),ncol=4)
  rd<-dfm$LickData[,c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12")]
  rd2<-dfm$BaselineData[,c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12")]
  chamber.sets<-dfm$Parameters$Chamber.Sets
  for(i in 1:6) {
    w1<-chamber.sets[i,1]
    w2<-chamber.sets[i,2]
    l1<-rd[,w1]
    l2<-rd[,w2]
    both<-l1&l2
    
    if(sum(both)>0){
      tmp<-rd2[both,]
      tmp2<-tmp[,c(w1,w2)]
      tmp3<-apply(tmp2,1,min)
      lick.matrix[i,]<-c(sum(l1),sum(l2),sum(both),max(tmp3))
    }
    else {
      lick.matrix[i,]<-c(sum(l1),sum(l2),0,0)
    }
  }
  colnames(lick.matrix)<-c("Col1","Col2","Both","MaxMin")
  rownames(lick.matrix)<-c("Row1","Row2","Row3","Row4","Row5","Row6")
  
  lick.matrix
}

CheckForBleeding.DFM<-function(dfm,cutoff){
  avg.matrix<-matrix(rep(NA,144),ncol=12)
  for(i in 1:12) {
    rd<-dfm$BaselineData[,c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12")]
    tmp<-rd[rd[,i]>cutoff,]
    averages<-apply(tmp,2,mean)  
    avg.matrix[i,]<-averages
  }
  avg.matrix[is.nan(avg.matrix)]<-0
  result<-list(Matrix = avg.matrix)
  rd<-dfm$BaselineData[,c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12")]
  result$AllData<-averages<-apply(rd,2,mean)  
  rownames(result$Matrix)<-c("W1Sig","W2Sig","W3Sig","W4Sig","W5Sig","W6Sig","W7Sig","W8Sig","W9Sig","W10Sig","W11Sig","W12Sig")
  colnames(result$Matrix)<-c("W1Resp","W2Resp","W3Resp","W4Resp","W5Resp","W6Resp","W7Resp","W8Resp","W9Resp","W10Resp","W11Resp","W12Resp")
  
  ## Now for plotting
  plot.matrix<-avg.matrix
  for(i in 1:12){
    plot.matrix[i,i]<-0
  }
  #filled.contour(t((avg.matrix)),asp=1.0,xlim=c(0,1),ylim=c(0,1), levels=c(0,5,10,15,20,25,30, 500))
  filled.contour(t((plot.matrix)),asp=1.0,xlim=c(0,1),ylim=c(0,1),levels=c(0,5,10,15,20,25,30,500))
  
  result
}

## This function will look for consecutive entries in the
## RawData$Sec column whose difference is larger than it
## should be based on the Samples.Per.Sec parameter.
FindDataBreaks<-function(dfm,multiplier=4,returnvals=TRUE){
  Interval<-diff(dfm$RawData$Seconds)
  Interval<-c(0,Interval)
  thresh<-(1.0/dfm$Parameters$Samples.Per.Second)*multiplier
  Index<-1:length(Interval)
  Index<-Index[Interval>thresh]
  Interval<-Interval[Interval>thresh]
  if(returnvals==TRUE) {
    if(length(Interval)==0)
      c(NA)
    else
      cbind(Index,Interval,dfm$RawData[Index,])  
  }
  else {
    if(length(Interval)==0)
      c(NA)
    else
      c(1)
  } 
  
}

FindDataBreaksV3<-function(dfm,multiplier=4,returnvals=TRUE){
  diffs<-diff(dfm$RawData$Index)
  diffs<-c(0,diffs)
  thresh<-1
  Index<-1:length(diffs)
  Index<-Index[diffs>thresh]
  diffs<-diffs[diffs>thresh]
  if(returnvals==TRUE) {
    if(length(diffs)==0)
      c(NA)
    else
      cbind(Index,diffs,dfm$RawData[Index,])  
  }
  else {
    if(length(diffs)==0)
      c(NA)
    else
      c(1)
  } 
}

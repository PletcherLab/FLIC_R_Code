rm(list=ls())
source("CommonChamber.R")
source("DFM.R")
source("MiscFunctions.R")
source("CameraSync.R")

p<-ParametersClass.SingleWell()
p<-SetParameter(p,Feeding.Event.Link.Gap=100)
dfm<-DFMClass(1,p)

OutputEventPlots(dfm,5)

tmp<-dfm$LickData$W1



Link.Events<-function(z,thresh){
  zz<-z
  tmp<-rle(z)
  for(i in 1:(length(tmp$lengths)-1){
    if(tmp$values[i]==FALSE && tmp$lengths[i]<thresh){
      zz[tmp$lengths[i]:tmp$lengths[i+1]-1]<-TRUE
    }
  }
  zz
}

Link.Events<-function(z,thresh){
  tmp<-rle(z)
  result<-c(FALSE)
  for(i in 1:length(tmp$lengths)){
    if(tmp$values[i]){
      tmp2<-rep(TRUE,tmp$lengths[i])
      result<-c(result,tmp2)
    }
    else {
      if(tmp$lengths[i]>thresh){
        tmp2<-rep(FALSE,tmp$lengths[i])
        result<-c(result,tmp2)
      }
      else {
        tmp2<-rep(TRUE,tmp$lengths[i])
        result<-c(result,tmp2)
      }
    }
  }
  result[-1]
}
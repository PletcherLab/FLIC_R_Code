rm(list=ls())
source("CommonChamber.R")
source("DFM.R")
source("MiscFunctions.R")


p<-ParametersClass.SingleWell()
dfm<-DFMClassV3(1,p)
dfm<-DFMClassV3(2,p)
dfm<-DFMClassV3(3,p)






## Testing the Two Well Data with Example
expDesign<-read.csv("ExpDesign.csv")
p.choice.one<-ParametersClass.TwoWell()
p.choice.two<-ParametersClass.TwoWell()
p.choice.two<-SetParameter(p.choice.two,PI.Multiplier=-1.0)
monitors.choice<-c(11,12,13,14,15,16,17)
p.choice.list<-list(p.choice.two,p.choice.one,p.choice.two,p.choice.one,p.choice.two,p.choice.one,p.choice.two)

tmp<-Feeding.Summary.Monitors(monitors.choice,p.choice.list,expDesign)

PlotLicksandLight.Well(DFM11,7)
PlotLicksandLight.Well(DFM11,8)
PlotLicksandLight.Well(DFM11,7,range=c(102,103))
PlotLicksandLight.Well(DFM11,8,range=c(102,103))
                        

p<-ParametersClass.SingleWell()
p<-SetParameter(p,Feeding.Event.Link.Gap=10000)
dfm<-DFMClass(4,p)



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
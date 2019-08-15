## Private Class Methods for FLIC Class V2
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)


#####Treatment based functions######
Feeding.BinnedLicksPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingLicks.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    FeedingLicks.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}



monitors<-c(2,3,4,5)
p2<-ParametersClass.TwoWell()
p<-ParametersClass.SingleWell()
ed<-read.csv("ExpDesign.csv")
tmp<-BinnedFeeding.Summary.Monitors(monitors,p,10,ed)

tmp<-tmp$Stats

pd <- position_dodge(0.8) # move them .05 to the left and right
gp<-ggplot(tmp,aes(x=Min,y=Licks,color=Treatment)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +geom_point(position=pd, size=3, shape=21, fill="white")
show(gp)














#####Binning functions######


#####Private functions######










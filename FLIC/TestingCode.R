rm(list=ls())
source("CommonChamber.R")
source("DFM.R")
source("MiscFunctions.R")


p1<-ParametersClass.SingleWell()
p2<-ParametersClass.TwoWell()
dfm1<-DFMClass(1,p1)
dfm2<-DFMClass(2,p1)
dfm2<-DFMClass(3,p1)
dfm2<-DFMClass(4,p1)


dfm<-DFMClass(11,p2)
dfm<-DFMClass(12,p2)
dfm<-DFMClass(13,p2)
dfm<-DFMClass(14,p2)
dfm<-DFMClass(15,p2)
dfm<-DFMClass(16,p2)
dfm<-DFMClass(17,p2)

expDesign<-read.csv("ExpDesign.csv")

monitors<-c(1,2,3,4)
monitors2<-c(11,12,13,14,15,16,17)


fs1<-Feeding.Summary.DFM(dfm1)
fs2<-Feeding.Summary.DFM(dfm2)

bfs1<-BinnedFeeding.Summary.DFM(dfm1,60)
bfs2<-BinnedFeeding.Summary.DFM(dfm2,60)

bfs1[bfs1$Chamber==1,]
bfs2[bfs2$Chamber==1,]

fsm<-Feeding.Summary.Monitors(monitors,p1,expDesign = expDesign)
bfsm<-BinnedFeeding.Summary.Monitors(monitors,p1,60,expDesign = expDesign)


fsm2<-Feeding.Summary.Monitors(c(11,12,13,14),p2,expDesign = expDesign)
bfsm2<-BinnedFeeding.Summary.Monitors(c(11,12,13,14),p2,60,expDesign = expDesign)

BinnedPlot.OneWell.Trt(monitors,p1,60,ExpDesign)
BinnedPlot.OneWell.Trt(bfsm2)



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





BinnedPlot.OneWell.Trt<-function(monitors,parameters,binsize.min=30,expDesign,range=c(0,0),Type="Licks",SaveToFile=FALSE,TransformLicks=TRUE){
  if(parameters$Chamber.Size!=1)
    stop("This function is for one chamber DFM only")
  
  data<-BinnedFeeding.Summary.Monitors(monitors,parameters,binsize.min,expDesign,range,SaveToFile,TransformLicks)  
  tmp<-data$Stats
  pd <- position_dodge(5) # move them .05 to the left and right
  
  
  
  if(Type=="Licks") {
    if(TransformLicks==TRUE){
      ylabel<-"Transformed Licks"
    }
    else {
      ylabel<-"Licks"
    }
    gp<-ggplot(tmp,aes(x=Min,y=Licks,color=Treatment,group=Treatment)) + 
      geom_errorbar(aes(ymin=Licks-LicksSEM, ymax=Licks+LicksSEM,color=Treatment), width=.1, position=pd) +
      geom_line(position=pd,size=1) +
      geom_point(position=pd, size=4, shape=21, fill="white") +xlab("Minutes") + ylab(ylabel)
  }
  else if(Type=="Licks") {
    
  }
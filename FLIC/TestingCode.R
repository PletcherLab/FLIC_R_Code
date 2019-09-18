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

tmp<-Feeding.Summary.Monitors(monitors.choice,p.choice.list,expDesign)

Lights<-GetLightsInfo(DFM11)

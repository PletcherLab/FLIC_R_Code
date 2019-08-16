rm(list=ls())
source("CommonChamber.R")
source("ChoiceFunctions.R")
source("DFM.R")
source("MiscFunctions.R")

## Testing the Two Well Data with Example
p2<-ParametersClass.TwoWell()
ed<-read.csv("ExpDesign.csv")
monitors<-c(2,3,4,5,6,8)

fsummary<-Feeding.Summary.Monitors(monitors,p2,ed)
binfsummary<-BinnedFeeding.Summary.Monitors(monitors,p2,binsize.min=60,ed)
Feeding.EventsPlot.Trt(monitors,p2,ed,divisions=4)
Feeding.LicksPlot.Trt(monitors,p2,ed,divisions=4)
R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 

p2<-ParametersClass.TwoWell()
p2<-SetParameter(p2,PI.Multiplier=-1)
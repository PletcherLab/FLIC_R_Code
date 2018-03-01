rm(list=ls())
source("TwoWellChamber.R")
source("SingleWellChamber.R")



##attach("FLICFUNCTIONS",2)

monitors<-c(3,4,6,8,11)
p<-ParametersClass.SingleWell()

ed<-read.csv("ExpDesign.csv")

for(i in monitors)
  DFMClass(i,p)



results<-FeedingLicks.Trt(monitors,p,ed,divisions=3,SaveToFile=TRUE)
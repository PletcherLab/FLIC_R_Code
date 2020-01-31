rm(list=ls())
source("CommonChamber.R")
source("DFM.R")
source("MiscFunctions.R")
source("CameraSync.R")

p<-ParametersClass.SingleWell()
p<-SetParameter(p,Feeding.Threshold = 10,Feeding.Minimum = 2)
dfm<-DFMClass.LinkFiles(1,p)
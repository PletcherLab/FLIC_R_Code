## Used to make the file for distribution

rm(list=ls())
source("CommonChamber.R")
source("MiscFunctions.R")
source("DFM.R")
source("ParametersClass.R")
save.image(file="FLICFunctions")

p<-ParametersClass.SingleWell()
dfm<-DFMClass(2,p)

p2<-ParametersClass.TwoWell()
dfm2<-DFMClass(2,p2)
## Used to make the file for distribution

rm(list=ls())
source("TwoWellChamber.R")
source("SingleWellChamber.R")
save.image(file="FLICFunctions")

p<-ParametersClass.SingleWell()
dfm<-DFMClass(2,p)

p2<-ParametersClass.TwoWell()
dfm2<-DFMClass(2,p2)
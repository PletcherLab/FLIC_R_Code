## Private Class Methods for FLIC Class V2
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)


#####Treatment based functions######





monitors<-c(2,3,4,5)
p2<-ParametersClass.TwoWell()
ed<-read.csv("ExpDesign.csv")


tmp<-BinnedFeeding.Summary.Monitors(monitors,p2,30,ed)  








#####Binning functions######


#####Private functions######










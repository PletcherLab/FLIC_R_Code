## Testing the new V3.5 version of the code

rm(list=ls())
source("FLICUserFunctions.R")
source("MiscFunctions.R")

p1<-ParametersClass.SingleWell()
monitors<-c(1,3)
results<-Feeding.Summary.Monitors.BySingleEvents(monitors,p1,events=c(2,5))


tmp<-read.csv("eventtest.csv")
tmp<-separate(data = tmp, col = Treatment, into = c("A", "B","C"))


p1<-ParametersClass.SingleWell()
p2<-ParametersClass.TwoWell()

p.choice.one<-ParametersClass.TwoWell()
p.choice.two<-ParametersClass.TwoWell()
p.choice.two<-SetParameter(p.choice.two,PI.Multiplier=-1.0)
monitors.choice<-c(11,12,13,14,15,16,17)
p.choice.list<-list(p.choice.two,p.choice.one,p.choice.two,p.choice.one,p.choice.two,p.choice.one,p.choice.two)



dfm1<-DFMClass(1,p2)
dfm2<-DFMClass(2,p1)
dfm3<-DFMClass(3,p2)
dfm2<-DFMClass(4,p1)


dfm<-DFMClass(11,p.choice.two)
dfm<-DFMClass(12,p.choice.one)
dfm<-DFMClass(13,p.choice.two)
dfm<-DFMClass(14,p.choice.one)
dfm<-DFMClass(15,p.choice.two)
dfm<-DFMClass(16,p.choice.one)
dfm<-DFMClass(17,p.choice.two)

rm(p.choice.one)
rm(p.choice.two)

expDesign<-read.csv("ExpDesign.csv")

monitors<-c(1,2,3,4)
monitors2<-c(11,12,13,14,15,16,17)

fsm<-Feeding.Summary.Monitors(monitors,p1,expDesign = expDesign)
bfsm<-BinnedFeeding.Summary.Monitors(monitors,p1,60,expDesign = expDesign)

fsm2<-Feeding.Summary.Monitors(monitors2,p.choice.list,expDesign = expDesign)
bfsm2<-BinnedFeeding.Summary.Monitors(monitors2,p.choice.list,60,expDesign = expDesign)

DataPlot(fsm,Type="Licks")
DataPlot(fsm,Type="Events")
DataPlot(fsm,Type="Durations")
DataPlot(fsm,Type="MinInt")
DataPlot(fsm,Type="TimeBtw")

DataPlot(fsm2,Type="Licks")
DataPlot(fsm2,Type="Events")
DataPlot(fsm2,Type="Durations")
DataPlot(fsm2,Type="MinInt")
DataPlot(fsm2,Type="TimeBtw")

BinnedDataPlot(bfsm,Type="Licks")
BinnedDataPlot(bfsm,Type="Events")
BinnedDataPlot(bfsm,Type="Durations")
BinnedDataPlot(bfsm,Type="MinInt")
BinnedDataPlot(bfsm,Type="TimeBtw")

BinnedDataPlot(bfsm2,Type="Licks")
BinnedDataPlot(bfsm2,Type="Events")
BinnedDataPlot(bfsm2,Type="Durations")
BinnedDataPlot(bfsm2,Type="MinInt")
BinnedDataPlot(bfsm2,Type="TimeBtw")

DivisionPlots.Monitors(monitors,p1,expDesign,divisions=1,Type="Licks",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=1,Type="Licks",SaveToFile=FALSE,TransformLicks=TRUE)

DivisionPlots.Monitors(monitors,p1,expDesign,divisions=4,Type="Licks",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=4,Type="Licks",SaveToFile=FALSE,TransformLicks=TRUE)


DivisionPlots.Monitors(monitors,p1,expDesign,divisions=1,Type="Events",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=1,Type="Events",SaveToFile=FALSE,TransformLicks=TRUE)

DivisionPlots.Monitors(monitors,p1,expDesign,divisions=4,Type="Events",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=4,Type="Events",SaveToFile=FALSE,TransformLicks=TRUE)


DivisionPlots.Monitors(monitors,p1,expDesign,divisions=1,Type="Durations",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=1,Type="Durations",SaveToFile=FALSE,TransformLicks=TRUE)

DivisionPlots.Monitors(monitors,p1,expDesign,divisions=4,Type="Durations",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=4,Type="Durations",SaveToFile=FALSE,TransformLicks=TRUE)


DivisionPlots.Monitors(monitors,p1,expDesign,divisions=1,Type="MinInt",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=1,Type="MinInt",SaveToFile=FALSE,TransformLicks=TRUE)

DivisionPlots.Monitors(monitors,p1,expDesign,divisions=4,Type="MinInt",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=4,Type="MinInt",SaveToFile=FALSE,TransformLicks=TRUE)

DivisionPlots.Monitors(monitors,p1,expDesign,divisions=1,Type="TimeBtw",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=1,Type="TimeBtw",SaveToFile=FALSE,TransformLicks=TRUE)

DivisionPlots.Monitors(monitors,p1,expDesign,divisions=4,Type="TimeBtw",SaveToFile=FALSE,TransformLicks=TRUE)
DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=4,Type="TimeBtw",SaveToFile=FALSE,TransformLicks=TRUE)

OutputData.Monitors(monitors,p1,expDesign,Type="BaselinedData")
OutputData.Monitors(monitors2,p.choice.list,expDesign,Type="BaselinedData",filename="Choice")

OutputData.Monitors(monitors,p1,expDesign,Type="Durations")
OutputData.Monitors(monitors2,p.choice.list,expDesign,Type="Durations",filename="DChoice")

OutputData.Monitors(monitors,p1,expDesign,Type="TimeBtw")
OutputData.Monitors(monitors2,p.choice.list,expDesign,Type="TimeBtw",filename="TBChoice")

OutputData.Monitors(monitors,p1,expDesign,Type="TotalFeeding")
OutputData.Monitors(monitors2,p.choice.list,expDesign,Type="TotalFeeding",filename="TFChoice")


DivisionPlots.Monitors(monitors2,p.choice.list,expDesign,divisions=4,Type="EventPI",SaveToFile=FALSE,TransformLicks=TRUE)

CumulativeEventPIPlots(monitors2,p.choice.list,expDesign)


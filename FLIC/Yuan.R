rm(list=ls())

#source("FLICFunction.R")

attach("FLICFunctions",2)

p<-ParametersClass.SingleWell()

monitors<-c(2,3,4,7,9,12,13,14,17,19,22,23,24,27,29,32,33,34,37,39)

exp.design<-read.csv("ExpDesign.csv")

CheckData<-function(){
  ##temp = list.files(pattern="DFM_[0-9]*.csv")
  for(i in monitors){
    dfm<-DFMClass(i,p)
    RawDataPlot.DFM(dfm)
    cat ("Press [enter] to continue")
    line <- readline()
  }
}

CheckData()

## DFM12 Well 12 doesn't look good, maybe also well 10
## DFM13 well 11 shows no drinking, maybe fly was dead?
## DFM 22 well 12 has a high baseline with tons of signal, is this real?
## Also no drinking in DFM 23 Well 7. Strange, tons of signal in DFM 23 Well 4.
## Tons (too much?) drinking in DFM24 well 2, together with a high baseline, especially late in the experiment
## DFM 32 Well 12, baseline problems.
## Massive long dring in DFM34 well 6, like 10min!  PRobably not real.
## Massive long dring in DFM39 well 6, like 20min!  PRobably not real.


## Just glancing at the data, I think a feeding threshold of 150 is too high, so 
## I will start by setting it around 90.
p<-SetParameter(p,Feeding.Threshold.Value=90)
p<-SetParameter(p,Feeding.Interval.Minimum=30)
p<-SetParameter(p,Tasting.Threshold.Interval=c(10,30))
p<-SetParameter(p,Signal.Threshold=10)
p<-SetParameter(p,Feeding.Minevents = 2)
p

## Now need to update the DFM objects using this new set of parameters
## This can be handled by the DFM class, which will check to see what parameters
## have changed and adjust accordingly.
for(i in monitors){
  print(i)
  DFMClass(i,p)
}


## I think we need to use the range parameter here because
## the DFMs are run for different lengths of time.
Feeding.Summary.Monitors(monitors,p,exp.design, range=c(0,30))

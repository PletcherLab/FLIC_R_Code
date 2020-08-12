## Here is a workaround that will do what you want to plot only some treatments (I think).

IsolateTreatments<-function(x,treatments){
  tmp<-x$Results
  tmp2<-x$Stats
  tmp<-tmp[tmp$Treatment %in% treatments,]
  tmp2<-tmp2[tmp2$Treatment %in% treatments,]
  #tmp<-subset(tmp,tmp$Treatment==treatments)
  #tmp2<-subset(tmp2,tmp2$Treatment==treatments)
  results<-list(Results=tmp,Stats=tmp2)
  results
}


## Get the summary information as normal from Feeding.Summary.Monitors or Binned.Feeding.Summary.Monitors

## For example,
fsm<-Feeding.Summary.Monitors(monitors,p1,expDesign = expDesign)
bfsm<-BinnedFeeding.Summary.Monitors(monitors,p1,60,expDesign = expDesign)

## Then use the above function to isolate specific treatments and assign
## the result to a new data frame.

## Lets say you want only treatments "A" and "B".

fsm.2<-IsolateTreatments(fsm,c("A","B"))
bfsm.2<-IsolateTreatments(fsm,c("A","B"))

## Then pass this to the data plot (or binnded data plot)

DataPlot(fsm.2,type="Durations")
BinnedDataPlot(bfsm.2,type="Durations")

## I haven't had the chance to test this so I hope it works.  Let me know.
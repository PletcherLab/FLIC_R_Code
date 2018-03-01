## Brian

rm(list=ls())


attach("FLICFunctions",2)


p<-ParametersClass.SingleWell()

dfm<-DFMClass(5,p)
RawDataPlot.DFM(dfm)
BaselineDataPlot.DFM(dfm)
rm(DFM5)
#Something amiss with well 9.  Others look okay.

dfm<-DFMClass(7,p)
## If you get the message that there are data breaks, use the FindDataBreaks function to
## determine where
FindDataBreaks(dfm)
RawDataPlot.DFM(dfm)
BaselineDataPlot.DFM(dfm)
rm(DFM7)
## Several wells look problematic here: 1,2,4,9

dfm<-DFMClass(8,p)
RawDataPlot.DFM(dfm)
BaselineDataPlot.DFM(dfm)
## More problems: 4, 12
rm(DFM8)

dfm<-DFMClass(9,p)
RawDataPlot.DFM(dfm)
BaselineDataPlot.DFM(dfm)
## More problems: 2

## Noting the wells that look strange, you can output the summary data to a file
Feeding.Summary.Monitors(c(5,7,8,9),p)

## if you want to constrain the analysis to a particular time interval,
## pass a range option to the function with the interval in minutes
## since the start of the experiment. For example to get the data output
## to a file using only data collected from 500-1000 minutes after the start
## of the experiment.
Feeding.Summary.Monitors(c(5,7,8,9),p,range=c(500,1000))

## You might also relax the feeding criteria a bit to ensure that your results 
## are not sensitive to this issue.

## Make sure to change the name of the file that was created with the last function
## or it will be overwritten.

p.liberal<-SetParameter(p,Feeding.Threshold.Value=100,Feeding.Interval.Minimum=20,Signal.Threshold = 20)
Feeding.Summary.Monitors(c(5,7,8,9),p.liberal)



## if you want to get binned data for a circadian plot of some sort then use that function.
## it accepts the range option as well.
BinFeedingData.Licks(dfm,30)
BinFeedingData.Events(dfm,30)

## To output to a file: 30min bins
OutputBinnedFeeding.Monitors(c(5,7,8,9),p,30)


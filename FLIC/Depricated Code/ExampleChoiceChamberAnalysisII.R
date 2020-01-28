rm(list=ls())
require(ggplot2)
#source("FLICFunction.R")

attach("FLICFunctions",2)

p2<-ParametersClass.TwoWell()
dfm<-DFMClass(3,p2)
dfm<-DFMClass(6,p2)
dfm<-DFMClass(8,p2)
dfm<-DFMClass(9,p2)

monitors<-c(3,6,9,8)

ed<-read.csv("ExpDesign.csv")

RawDataPlot.DFM(DFM3)
## Chamber 1 is not awesome, but probably okay.Chamber 3 looks quite
## variable.  Might be some issue with the Board.

RawDataPlot.DFM(DFM6)
## Chamber 1 is a problem and should probably be ommitted.Cganber 2 has a high baseline in one well.

RawDataPlot.DFM(DFM9)
## No signal in Chmabers 1 adn 4 and in one well of Chamber 2, so these should probably be ignored as well

RawDataPlot.DFM(DFM8)
# No signal in Chamber 6 well A.  Maybe a problem, maybe not.

## So concerning chambers are:
## DFM3: 1, 3
## DFM6: 1, 2
## DFM9: 1, 4, 2
## DFM8: 6



BaselineDataPlot.DFM(DFM3)
BaselineDataPlot.DFM(DFM6)
BaselineDataPlot.DFM(DFM8)
BaselineDataPlot.DFM(DFM9)

## Based on the deisgn file that Yuan sent to me, 
## for the first run, we will ignore only these
## Not sure I agree with this, but this will be okay for demonstration.
## DFM8: 2,4,6
## DFM9: 1, 4
## These can be ignored in the Treatment analyses by not including them in the ExpDesign data frame

ed

#Note that you can extract all kinds of data from individual chambers or DFM using existing functions.
Feeding.Summary(DFM3)

## Or for groups of DFM using functions that end in .Monitors.  Many of these output (or have the option to output)
## text or pdf files.
Feeding.Summary.Monitors(monitors,p2)


GetIntervalData.DFM(DFM3)

## Note, also in nearly all cases you can focus the analysis to a specific time range, presented in minutes.
GetIntervalData.DFM(DFM3,range=c(0,100))
BaselineDataPlot.DFM(DFM3,range=c(0,100))
Feeding.ThresholdPlots.DFM(DFM3,range=c(0,200))


## For outputs that simply append the treatment to the data frame, the treatment will be given as "None"
## For example, several summary outputs now can take an experimental design frame as as optional argument
## If it is there, then you will get the additional column for easier use in Excel.
monitors<-c(3,6,9,8)
Feeding.Summary.Monitors(monitors,p2,ed)
OutputIntervalData.Monitors(monitors,p2,ed)
OutputTotalFeeding.Monitors(monitors,p2,ed)


## For these general summary data, I am hoping that just the addition of this one
## column should speed up the analysis quite a bit.

## For choice experiments, I have started expanding the PI type analysis, mostly exploratory
## to make general investigation of your data faster.  This will still need some time to 
## get into publication quality form, but hopefully that will only be required for a fraction of the 
## actual analyses that are done.

## So let's say we are interested in PI.  This is handled mostly by the functions in 
## TwoWellChamber.R.  If you have ever done a FLIC choice analysis, 
## you should have a copy of this file and should read through the comments
## and have tried nearly all of the functions.

## As is the general pattern, functions ending in .Chamber are executed on individual chambers
## functions ending in .Monitors are executed across every chamber in each DFM listed, and functions without
## either ending (or ending in .DFM) act on each of the 6 (or 12) chambers in a DFM.


## Some Chamber functions
## Get all PI data (FeedingPI, Feeding.EventPI, and Tasting)
PIData.Chamber(DFM3,1)

Feeding.FinalPI.Chamber(DFM3,2)
Feeding.FinalPI.Chamber(DFM3,2,range=c(0,100))

Feeding.PIPlots.Chamber(DFM6,2)
Feeding.PIPlots.DFM(DFM6)
Feeding.EventPIPlots.DFM(DFM6)

## You can also make Time Dependent and BoutPI plots for DFM
Feeding.TimeDependentPIPlots.Chamber(DFM3,1,window.size.min=30,step.size.min=3)
Feeding.TimeDependentPIPlots.DFM(DFM3,window.size.min=30,step.size.min=3)

## Currently the BoutPI functions will give an error when there are literatlly
## zero feeding events in a chamber.  I need to fix this.

## Every monitor in this group has a well with this, so I can't show you how this plot works.
## Will need to do that using a different set.
Feeding.BoutPIPlot.DFM(DFM8,3)

## You can also execute these functions at the DFM level (using functions that end in .Montors)
## and this will normally allow you to save the data in excel compatible files or PDFs of the plots

TimeDependentPI.Monitors(monitors,p2)
BoutPI.Monitors(monitors,p2,minEvents=3)

## These were useful, but there was still a bit of work to do until you could combine treatments to look at the data
## We are still unsure about how best to combine them officially, but I spent some time creating functions
## that should allow at least a faster examination of the data by treatment.

## Some functions (like the general ones mentioned above), just take the experimental design frame as an argument and 
## append the treatment name in a new column so you can quickly use a pivot table in excel.
TimeDependentPI.Monitors(monitors,p2,ed)
BoutPI.Monitors(monitors,p2,minEvents=3,ed)
BoutEventPI.Monitors(monitors,p2,minEvents=3,ed)


## New functions end in .Trt if they require an experimental design frame and somehow use it 
## to summarize the results. Dot plots of final PI to compare across treatments.
Feeding.FinalPIPlot.Trt(monitors,p2,ed)

## You can look at cumulative PI at several points during the experiment by
## dividing it up by divisions.
Feeding.FinalPIPlot.Trt(monitors,p2,ed,divisions=1)
Feeding.FinalEventPIPlot.Trt(monitors,p2,ed,divisions=3)


tmp<-Feeding.PIPlots.Trt(monitors,p2,ed)
tmp<-Feeding.EventPIPlots.Trt(monitors,p2,ed)

tmp<-Feeding.TimeDependentPIPlots.Trt(monitors,p2,ed)

tmp<-Feeding.BoutPI.Trt(monitors,p2,3,ed)
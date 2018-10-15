#####################################################################
#####################################################################
## This file contains some examples that will hopefully 
## make it easier to get started using the FLIC R Code.

## There are MANY additional functions that allow you to explore the details of
## your FLIC data.  We don't have documentation for them yet, but you can obtain the
## source code from the FLIC wiki, which contains some comments.  You can also 
## contact Scott (spletch@umich.edu) if you have specific questions or would
## like to interrogate your data in a specific way. 

## We are currently developing similar R functions for those interesting in analyzing data 
## base on a Single Chamber - Single Feeding Well setup. These are, unfortuantely, not done
## yet.  Contact Scott if you require assistance.

## This example file will continue to grow and provide some idea of how we have
## currently implemented data analysis.  Check back to the wiki often for
## updates.

## Created 7/23/2014 - Scott Pletcher
## Updated 7/25/2014 - Scott Pletcher
## Updated 3/18/2017 - Scott Pletcher

#####################################################################
#####################################################################

## Getting started

## Download R studio (link available on the wiki) and install it.
## Create a project directoryfolder on your hard drive.  Place your DFM data
## files in that directory (e.g., "DFM_1.csv").  Download the "ExampleChoiceChamberAnalysis.R" file 
## and the "TwoWellChamberFunctions" R code file from the wiki and place them in the project folder.

## start R studio. Choose File|New Project and click "Existing Directory."  Point to your
## project folder.  Once RStudio starts, open the "ExampleChoiceChamberAnalysis.R".  
## To execute the code below, you can either copy and paste from the file into the RConsole or 
## (on a PC) highlight the code and type Cntr-R to submit the region (I don't know the keyboard
## shortcut on a MAC).The functions used below for illustration are "Wrapper" functions that 
## do alot of analysis and output the results to PDF and/or CSV files that can be opened
## in other programs.

## Attach the R image that contains the relevant functions.
attach("FLICFunctions",pos=2)

## Version 2.2 requires a couple packages
require(ggplot2)
require(stats)

## While your current working environment is empty...
ls()

## you can get an idea of all of the functions that are currently distributed for 
## the FLIC analysis...
ls(2)


## This example will assume three that you have data from only a single DFM
## with ID=1. if you have more, say DFMs 1,3, and 5, you can simply
## replace 'c(1)' with 'c(1,3,5)' in the example code below. 

## First, to get an overview of the data, look at a feeding summary.
## This will also execute the calculations for each DFM needed for
## further analysis.

## First we need to create the parameters used for the analysis.
## For a choice experiment, we start with the default Two-Well Chamber
## set up.  These values should be examined for suitability in your data set.
p<-ParametersClass.TwoWell()

## You can observe the number of different parameters currently in use by typing
## the variable name
p

## Note that the feeding and tasting thresholds are critical in the determination of 
## all feeding interactions.  There is the option of a fixed or adaptive threshold.
## The adaptive threshold is computationally intensive and takes a good amount of time
## to calculate, particularly for large (e.g., over 5 hour) data sets.
## To change a parameter use the SetParameter function
p<-SetParameter(p,Feeding.Threshold.Value=30)
p<-SetParameter(p,Feeding.Interval.Minimum=20)
p<-SetParameter(p,Tasting.Threshold.Interval=c(10,20))
p

## If you have some DFM where the food choice is swapped between
## the left right channels (a good idea to randomize) then make sure
## the PI is adjusted for those DFM.  You can do this by changing
## the PI.Multiplier component of the parameter object.
## If you use this p2 parameter object when creating the DFM object (see below)
## then the direction of the PI will be swapped and the chamber values will be
## properly compared.
p2<-SetParameter(p,PI.Multiplier = -1)

## To avoid biasing your tasting data, make sure that 
## Tasting.Threshold.Interval.High <= Feeding.Interval.Minimum.

## For more information on the details of these algorithms, check out the source code
## or contact Scott.

## Contact Scott for information on the adaptive threshold, which
## is very sensitive and useful for circadian studies of feeding behavior.

## Let's say that we have data from four DFM.  These data files are all located
## in the R project directory.  We can read in the data and create dfm objects with
## the following code.
## These "Class" functions create a background dfm object (e.g., DFM3, DFM6, etc.)
## as well as return the object if you would like to assign it to a specific variable.
## In the following code, we just assign the results to a dummy variable (dfm) to suppress
## the output to the console.
dfm<-DFMClass(3,p)
dfm<-DFMClass(6,p)
dfm<-DFMClass(8,p2)
dfm<-DFMClass(9,p2)
rm(dfm)

## Note that the DFM objects were created in the background.
ls()

## Note also that DFM8 and DFM9 has the food sides switched
## so we have to us the PI.Multiplier=-1 to equate the direction
## of their PI related to DFM3 and DFM6. So we use the p2 parameter object.

## We will also create a vector that lists the DFM in our group for use
## later in functions that operate on multiple DFM.
monitors<-c(3,6,8,9)

## If you have an experimental design that you would like to incorporate
## into the analysis, read it in here.
## This file (and subsequent data frame) should have 3 columns
## (with names DFM, Chamber, and Treatment). Columns 1 and 2 should contain integer values
## denoting the DFM and Chamber number and the third column a short string noting the Treatment. 
ed<-read.csv("ExpDesign.csv")
ed

## Normally you would start by looking at the raw data to identify problematic
## chambers. For those that you would like to ignore in the analysis of treatments
## just delete them from the Deisgn data frame or set their treatment to "None"
RawDataPlot.DFM(DFM3)
RawDataPlot.DFM(DFM6)
RawDataPlot.DFM(DFM9)
RawDataPlot.DFM(DFM8)


## Global trends in the raw data can be eliminated after the baselines are calculated.
## So you should make sure that you examine those as well.
BaselineDataPlot.DFM(DFM3)
BaselineDataPlot.DFM(DFM6)
BaselineDataPlot.DFM(DFM8)
BaselineDataPlot.DFM(DFM9)


## So let's say we are interested in PI.  This is handled mostly by the functions in 
## TwoWellChamber.R.  
## You should have a copy of this file (downloadable from wikiflic.com) 
## and should have read through the comments
## and have tried nearly all of the functions.

## As a general pattern, functions ending in .Chamber are executed on individual chambers
## functions ending in .Monitors are executed across every chamber in each DFM listed, and functions without
## an ending (or ending in .DFM) act on each of the 6 (or 12) chambers in a DFM.

## You can extract all kinds of data from individual chambers or DFM using existing functions.
Feeding.Summary(DFM3)

## Or for groups of DFM using functions that end in .Monitors.  Many of these output (or have the option to output)
## text or pdf files.
## Because functions ending in monitors do not take DFM objects directly as arguments, parameters objects need to be 
## passed also.  Data will be calculated only if needed (e.g., if parameter values are different than those currently
## applied to the background DFM objects.)
## If only a single parameter object is passed, then it is applied to all of the monitors in the list.  In this case
## p is applied to monitors 3,6,8, and 9.  
Feeding.Summary.Monitors(monitors,p)

## But if, as we have done above, wish to use different parameters for each
## monitor, then you must provide a list of parameter objects of length equal to the length of monitors.
## For this next function the PI will be swapped by applying p2 to monitors 8 and 9.
Feeding.Summary.Monitors(monitors,list(p,p,p2,p2))

## Monitor functions like this iterate over the desired DFMs using the defined parameters and the entire
## range of the data.

## For Monitor functions, there is either a pdf generated with some graphical representation
## of each result and/or a csv file with the numerical output.  The csv file can be loaded
## directly into MSExcel. 

## A first pass analysis is useful to examine the raw data and the effectiveness of the
## baseline procedure.  This function produces large PDF files and should probably not
## be used if your experiment surpasses three or four hours of data.
## This will just combine the RawData and Baselined data into a pdf/
First.Pass.Analysis(c(1),p)

## Some Chamber functions
## Get all PI data (FeedingPI, Feeding.EventPI, and Tasting)
PIData.Chamber(DFM3,1)
Feeding.FinalPI.Chamber(DFM3,2)
Feeding.PIPlots.Chamber(DFM6,2)

## A couple DFM functions.
Feeding.PIPlots.DFM(DFM6)
Feeding.EventPIPlots.DFM(DFM6)

## For an idea of the preference index over the course of the experiment, the
## Time Dependent PI is a good measure.
## The two parameters of interest here are window size (which determines the number
## of minutes used for each PI estimate, and the step size, which determines at what
## resultion the PI values are generated.  Normally a window size of 30min and step size
## of 3 min is good for a 3 hour experiment using starved flies. If flies are not starved or
## feeding less then increase window and step size (e.g., 60min and 20min, respectively).
## You can execute these for individual chambers or for the DFM as a whole.  Or for 
## multiple DFM monitors
Feeding.TimeDependentPIPlots.Chamber(DFM3,1,window.size.min=30,step.size.min=3)
Feeding.TimeDependentPIPlots.DFM(DFM3,window.size.min=30,step.size.min=3)
TimeDependentPI.Monitors(monitors,list(p,p,p2,p2),window.size.min,step.size.min)

## Another measure of preference is defined by the number of feeding bouts that occur
## regardless of time. In other words, if the parameter minEvents=5, then a PI will
## be calculated for each sequential set of 5 feeding events, regardless of when they
## occur. Once these five-event intervals are defined, 
## the PI for each interval is calculated either according to the total number of interactions
## (Feeding.BoutPIPlot) or the total number of events (Feeding.BoutEventPIPlot). We find
## that the the Feeding Bout PI is most informative.

## We normally use minEvents=5 for a 3 hour experiment and minEvents=10 for longer experiments.
minEvents=5
Feeding.BoutPIPlot.DFM(DFM8,minEvents)
BoutPI.Monitors(monitors,p,minEvents)

## Note that for TimeDepenedent and BoutPI (as well as for general summary data)
## there is a distinction between PI and EventPI.  The PI uses individual interactions
## for the PI calculate (e.g., bites are the unit of measure).  The EventPI calculates
## the PI using a meal as the unit.  For example, if a fly drinks for 1 sec, it will generate
## five "licks" or positive signals (because the data are collected every 200ms), but because
## the licks are consecutive, this is considered one feeding event (e.g. meal). The EventPI thus
## descibed the meal preference rather than the total feeding preference.
BoutEventPI.Monitors(c(1,3,7),p,minEvents)


## Note, also in nearly all cases you can focus the analysis to a specific time range, presented in minutes.
GetIntervalData.DFM(DFM3,range=c(0,100))
BaselineDataPlot.DFM(DFM3,range=c(0,100))
Feeding.ThresholdPlots.DFM(DFM3,range=c(0,200))

## Most recently, we have incorporated the use of an Experimental Design
## data frame that will group Chambers from different DFM into the same
## Treatment and help present summaries across treatments.

## Some functions (like the general ones mentioned above), just take the experimental design frame as an 
## optional argument andappend the treatment name in a new column so you can quickly 
## use a pivot table in excel to calculate means and SD.
TimeDependentPI.Monitors(monitors,p2,ed)
BoutPI.Monitors(monitors,p2,minEvents=3,ed)
BoutEventPI.Monitors(monitors,p2,minEvents=3,ed)

## New functions end in .Trt (Again see the TwoWellChamber.R script file) if they require an experimental 
## design frame and somehow use it to summarize the results. Dot plots of final PI to compare across treatments.
Feeding.FinalPIPlot.Trt(monitors,p2,ed)

## You can look at cumulative PI at several points during the experiment by
## dividing it up by divisions. 
Feeding.FinalPIPlot.Trt(monitors,p2,ed,divisions=1)
Feeding.FinalEventPIPlot.Trt(monitors,p2,ed,divisions=3)

## You can also present and output data smoothed over treatments.
## Thin lines represent individual chambers for each treatment (color coded)
## while thick lines represent an lowess or gam smooth.
## These smoothed plots are almost surely not the right way to combine data across
## chambers (especially PI data), but they give you a general picture of the trends.
Feeding.PIPlots.Trt(monitors,p2,ed)
Feeding.EventPIPlots.Trt(monitors,p2,ed)

Feeding.TimeDependentPIPlots.Trt(monitors,p2,ed)

Feeding.BoutPI.Trt(monitors,p2,3,ed)


## There are many more functions, please look through all of the R files to get an idea.
## The functions for single well analysis (e.g. total feeding) are not nearly as well developed
## as those for preference analysis.  However, some functions exist to output the feeding data in 
## a way that makes it easy to do circadian analysis using existing software.

## Questions or suggestions, please let me know (spletch@umich.edu).


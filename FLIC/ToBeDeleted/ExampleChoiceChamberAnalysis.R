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
attach("TwoWellChamberFunctions",pos=2)

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
## To change a parameter use the ChangeParameter function
p<-SetParameter(p,Feeding.Threshold.Value=120)
p<-SetParameter(p,Feeding.Interval.Minimum=40)
p<-SetParameter(p,Tasting.Threshold.Interval=c(10,40))
p

## To avoid biasing your tasting data, make sure that 
## Tasting.Threshold.Interval.High <= Feeding.Interval.Minimum.

## For more information on the details of these algorithms, check out the source code
## or contact Scott.

## Contact Scott for information ont he adaptive threshold, which
## is very sensitive and useful for circadian studies of feeding behavior.

## This will iterate over the desired DFM using the defined parameters and the entire
## range of the data.

## For each of the functions below, there is a pdf generated with some graphical representation
## of each result and a csv file with the numerical output.  The csv file can be loaded
## directly into MSExcel. 


Feeding.Summary.Monitors(c(1),p)

## A first pass analysis is useful to examine the raw data and the effectiveness of the
## baseline procedure.  This function produces large PDF files and should probably not
## be used if your experiment surpasses three or four hours of data.
First.Pass.Analysis(c(1),p)


## For an idea of the preference index over the course of the experiment, the
## Time Dependent PI is a good measure.
## The two parameters of interest here are window size (which determines the number
## of minutes used for each PI estimate, and the step size, which determines at what
## resultion the PI values are generated.  Normally a window size of 30min and step size
## of 3 min is good for a 3 hour experiment using starved flies. If flies are not starved or
## feeding less then increase window and step size (e.g., 60min and 20min, respectively).
window.size.min=30
step.size.min=3
MovingPI.Monitors(c(1),p,window.size.min,step.size.min)


## Another measure of preference is defined by the number of feeding bouts that occur
## regardless of time. In other words, if the parameter minEvents=5, then a PI will
## be calculated for each sequential set of 5 feeding events, regardless of when they
## occur. Once these five-event intervals are defined, 
## the PI for each interval is calculated either according to the total number of interactions
## (Feeding.BoutPIPlot) or the total number of events (Feeding.BoutEventPIPlot). We find
## that the the Feeding Bout PI is most informative.

## We normally use minEvents=5 for a 3 hour experiment and minEvents=10 for longer experiments.
minEvents=5
BoutPI.Monitors(c(1),p,minEvents)
##BoutEventPI.Monitors(c(1,3,7),p,minEvents)

##############################################
## Other potentially useful functions.
## These functions will plot the cumulative PI (either Feeding/Tasting or
## based on feeding events rather that interactions) for a given DFM and 
## Chamber (from 1-6 corresponding to rows on the DFM).
## The plot will be shown in the RStudio plots window.

## For these (and many other) functions, you need to specify a specific dfm for analysis
## and the associated parameter values. The data file must be in the project directory
p<-ParametersClass.TwoWell()
dfm<-DFMClass(1,p)

## Let's choose the top row, chamber 1.
chamber<-1
Feeding.PIPlots.Chamber(dfm,chamber)
Tasting.PIPlots(dfm,chamber)
Feeding.EventPIPlots(dfm,chamber)

  
## For most function, you can focus your analysis on a particular range of the data
## specified in terms of minutes.  For example, to focus the analysis only on data
## from 10min-60min in the data file
range=c(10,60)
Feeding.PIPlots.Chamber(dfm,chamber,range)
Tasting.PIPlots(dfm,chamber,range)
Feeding.EventPIPlots(dfm,chamber,range)


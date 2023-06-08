## R code for simple FLIC analyses

### Version 4.0-beta (April 23, 2020)

In this repository you will find the latest R Code used by the Pletcher laboratory to analyze FLIC data. Functions are provided to examine interactions with food (termed 'licks' and 'events') for single-well and food choice experiments. 

#### (Version 4.0) 
The analysis code was greatly simplified to make it easier for new FLIC users to rapidly analyze data.  Naming schemes, particularly for front-end functions were simplified and the number of these functions was greatly reduced.  Unfortunately, this will also break most analysis scripts, although the backend code is largely unchanged. As a result some modest rewriting will likely be necessary to port code from Version 3.x to version 4.x. 

One noteworthy change in V4.0 is that a new parameter has been introduced: Feeding.Event.Link.Gap. This parameter will affect the number of reported Events (but not Licks). Contiguous periods of interaction (licks) will be combined into a single event if gaps of inactivity between them are less than this time (in seconds). This was added because video analysis of feeding in the FLIC revealed that what might subjectively be considered the same feeding bout occassionaly exhibited very short (< 400ms) periods of no food contact, effectively separating them into discrete Events in the results.  You can set this parameter to 0, to replicate Event data from previous versions.  We now use this set at 1sec (the default).

#### (Version 3.0) 
In August 2019, Scott Pletcher significantly revised the code to reduce the number of functions (many were never used) and to simplify the terminology. If you have used previous versions of this code, your scripts will almost surely not work with this version.  However, porting should be quite straightforward once you have read the documentation and understand the new naming convensions and reduced code base. 

For those of you who would like access to the old code, some of it can be found in the 'Depricated Code' folder in this repository. Alternatively, you can obtain all of the previous functions and scripts by cloning or downloading the v2.3 version of the repository (select it under the Tags drop down box).

If you have any questions or would like to contribute to this project, please contact [Scott pletcher](mailto:spletch@umich.edu).

There are three documentation files that you should use to get started, which should be read in the following order:

1. *GettingStarted.html*
2. *GroupedAnalysis.html*
3. *ChoiceChamberAnalysis.html*

### From *GettingStarted.html*

This document begins a series of markdown files that contain working examples that will hopefully make it easier to understand how we analyze the signal data from FLIC DFMs. We have written many R functions that we find useful for analyzing simple feeding experiments that involve one feeding well per chamber as well as choice experiments that involve two feeding wells per chamber.  These markdown files currently serve as the documentation.  The source code is (semi) commented and available from GitHub (https://github.com/PletcherLab/FLIC_R_Code). The example data files are available in the repository as an archived (i.e., zipped) file. You may also contact [Scott](mailto:spletch@umich.edu) if you have specific questions or would like to interrogate your data in a specific way that doesn't appear to be supported in the code. 

As with all FLIC R code, this is a work in progress. If you decide to extend this code and wish to share it with the community, please contact [Scott](mailto:spletch@umich.edu).

#### Getting Started
1. Download and install the latest version of [R](https://cran.r-project.org/) or [Microsoft Open R](https://mran.microsoft.com/open) for your operating system.

2. Download R studio (https://www.rstudio.com/) and install it. 

3. Create a new folder on your hard drive to serve as the project directory.  

4. Copy all of  DFM data files (e.g., "DFM_1.csv") to that folder.  

5. Clone the Github Respository or download the files as a zip file. Unzip or copy the .R files and/or the FLICFUNCTIONS file to the project directory. 

6. Start R studio. Choose File|New Project and click "Existing Directory."  Point to your project folder. R Studio will set the default directly to your project folder.  You are ready to start analyzing your feeding interaction data!

7. You will need to ensure that you have the following R packages installed: *ggplot2*, *stats*, *reshape2*, and *gridExtra*. If you are new to R, you can find lots of information about packages and installing them in RStudio with a simple web search (e.g., [here](https://www.datacamp.com/community/tutorials/r-packages-guide) and [here](http://web.cs.ucla.edu/~gulzar/rstudio/))

8. All relevant files used for the example analyses are provided in the *ExampleData.zip* file. To run the examples, you will need to extract these files into the project directory.


### Have fun!

Let us know if you have an interesting use for the FLIC or if you have a request for a new implementation

### Notes
Portions of this code were used in the analyses presented in Weaver et al., eLife, 2023 (DOI: https://doi.org/10.7554/eLife.84537.3)


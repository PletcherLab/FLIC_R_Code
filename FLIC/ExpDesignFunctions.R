## Functions that aid in the handling of Experimental Design

mySEM<-function(x){
  if(is.factor(x))
    tmp<-unique(as.character(x))
  
  else
    tmp<-sqrt(var(x)/length(x))
  tmp
}

GetTreatmentForChamber<-function(dfmNum,chamberNum,expdesign){
  tmp<-subset(expdesign,DFM==dfmNum)
  tmp<-subset(tmp,Chamber==chamberNum)
  if(nrow(tmp)==0)
    return("None")
  else
    return(as.character(tmp$Treatment))
}

AppendTreatmentonResultsFrame<-function(results,expdesign){
  Treatment<-rep(NA,nrow(results))
  for(i in 1:nrow(results)){
    Treatment[i]<-GetTreatmentForChamber(results$DFM[i],results$Chamber[i],expdesign)
  }
  n<-names(results)
  n<-c("Treatment",n)
  results<-cbind(Treatment,results)
  names(results)<-n
  results
}



AggregateTreatments<-function(results){
  trt.summary1<-aggregate(results,by=list(results$Treatment),mean) 
  trt.summary2<-aggregate(results,by=list(results$Treatment),mySEM)


  trt.summary<-rbind(trt.summary1,trt.summary2)
  trt.summary<-trt.summary[,-grep("Treatment|DFM|Chamber",colnames(trt.summary))]
  names(trt.summary)[names(trt.summary) == "Group.1"] <- "Treatment"
  trt.summary
}


## This function accepts a data frame that is assumed to 
## hold results with a column called treatment, that indicates
## treatment
AggregateTreatmentsOLD<-function(results){
  trt.summary1<-aggregate(results,by=list(results$Treatment),mean) 
  Stat<-rep("Mean",nrow(trt.summary1))
  trt.summary1<-data.frame(Stat,trt.summary1)
  trt.summary2<-aggregate(results,by=list(results$Treatment),mySEM)
  Stat<-rep("SEM",nrow(trt.summary2))
  trt.summary2<-data.frame(Stat,trt.summary2)
  trt.summary<-rbind(trt.summary1,trt.summary2)
  trt.summary<-trt.summary[,-grep("Treatment|DFM|Chamber",colnames(trt.summary))]
  names(trt.summary)[names(trt.summary) == "Group.1"] <- "Treatment"
  trt.summary
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
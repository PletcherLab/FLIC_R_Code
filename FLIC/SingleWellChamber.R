## Private Class Methods for FLIC Class V2
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)


#####Treatment based functions######
Feeding.BinnedLicksPlot.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingLicks.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    FeedingLicks.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}




















#####Binning functions######
GetCumLicksPlots.DFM<-function(dfm,FacetPlots=TRUE,TransformLicks=TRUE){
  tmp<-Feeding.Durations.Well(dfm,1)
  SumLicks<-cumsum(tmp$Licks)
  if(TransformLicks==TRUE)
    SumLicks<-SumLicks^0.25
  row<-1
  col<-1
  Row<-rep(row,nrow(tmp))
  Col<-rep(col,nrow(tmp))
  Well<-rep(1,nrow(tmp))
  tmp<-data.frame(tmp,SumLicks,Row,Col,Well)
  for(i in 2:12){
    if(i%%2==1) row<-row+1
    col<-col+1
    if(col>2) col<-1
    tmp2<-Feeding.Durations.Well(dfm,i)
    if(is.data.frame(tmp2)) {
      x<-tmp2$Minutes
      y<-cumsum(tmp2$Licks)
      SumLicks<-y
      if(TransformLicks==TRUE)
        SumLicks<-SumLicks^0.25
      Row<-rep(row,length(x))
      Col<-rep(col,length(x))
      Well<-rep(i,length(x))
      tmp2<-data.frame(tmp2,SumLicks,Row,Col,Well)
      tmp<-rbind(tmp,tmp2) 
    }
    else {
      fake.entry1<-c(FirstSampleData(dfm)$Minutes,0,0,0,0,0,0,0,0,row,col,i)
      fake.entry2<-c(LastSampleData(dfm)$Minutes,0,0,0,0,0,0,0,0,row,col,i)
      fake.entry<-data.frame(rbind(fake.entry1,fake.entry2))
      names(fake.entry)<-names(tmp)
      tmp<-rbind(tmp,fake.entry) 
    }
  }
  if(TransformLicks==TRUE)
    ylabel="Transformed Cumulative Licks"
  else
    ylabel="Cumulative Licks"
  if(FacetPlots==TRUE) {
  gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line() + facet_grid(rows=vars(Row),cols=vars(Col)) +geom_point() +
    ggtitle(paste("DFM",dfm$ID)) + ylab(ylabel)
  }
  else {
  gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line(size=1.2) +
    ggtitle(paste("DFM",dfm$ID))+ ylab(ylabel)
  }
  gp
}
GetTotalLicksPlot.Monitors<-function(monitors, p, range=c(0,0),TransformLicks=TRUE){
  tmp2<-Feeding.Summary.Monitors(monitors,p,range=range,TransformLicks)
  tmp2<-tmp2$Results
  ylabel="Licks"
  if(TransformLicks==TRUE) {
    ylabel="Transformed Licks"
  }
  tmp2$DFM<-factor(tmp2$DFM)
  gp<-ggplot(tmp2,aes(x=DFM,y=Licks,fill=DFM)) + geom_dotplot(binaxis='y',stackdir='center',stackratio=1.5, dotsize=0.7) +
    ylab(ylabel)
  gp
}


#####Private functions######










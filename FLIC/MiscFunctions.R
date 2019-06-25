require(ggplot2)
require(stats)
require(gridExtra)


GetCumLicksPlots.DFM<-function(dfm){
  tmp<-Feeding.Durations.Well(dfm,1)
  SumLicks<-cumsum(tmp$Licks)
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
    SumLicks<-cumsum(tmp2$Licks)
    Row<-rep(row,nrow(tmp2))
    Col<-rep(col,nrow(tmp2))
    Well<-rep(i,nrow(tmp2))
    tmp2<-data.frame(tmp2,SumLicks,Row,Col,Well)
    
    tmp<-rbind(tmp,tmp2)
  }
  gp<-ggplot(tmp,aes(Minutes,SumLicks,color=factor(Well))) + geom_line() + facet_grid(rows=vars(Row),cols=vars(Col)) +geom_point()
  gp
}


GetTotalLicksPlot.Monitors<-function(monitors, p, range=c(0,0)){
  tmp2<-Feeding.Summary.Monitors(monitors,p,range=range)
  tmp2<-tmp2$Results
  tmp2$DFM<-factor(tmp2$DFM)
  gp<-ggplot(tmp2,aes(x=DFM,y=Licks,fill=DFM)) + geom_dotplot(binaxis='y',stackdir='center',stackratio=1.5, dotsize=0.7)
  gp
}

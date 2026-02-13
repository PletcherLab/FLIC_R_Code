rm(list=ls())
source("FLICUserFunctions.R")
source("MiscFunctions.R")
library(dplyr)

Get.Well.And.Treatment<-function(id,chamber,design){
  for(i in 1:nrow(design)){
    if((design$DFM[i]==id) && (design$Chamber[i])==chamber){
      return(design[i,c("SucroseWell","Treatment2","Pair")])
    }
  }
  return (NA,NA,NA)
}

Get.Paired.DataFrame<-function(dfms,params,expdesign){
  for(ii in dfms){
    dfm<-DFMClass(ii,p1)
    if(ii == dfms[1]){
      PairedData<-data.frame(dfm$LickData$Minutes)  
      names(PairedData)<-c("Minutes")
    }
    for(i in 1:6){
      tmp<-Get.Well.And.Treatment(dfm$ID,i,exp.design)
      if(tmp$Treatment2=="Paired"){
        colname<-paste(tmp$Treatment2,tmp$Pair,sep="_")
        PairedData[[colname]] <- dfm$LickData[1:nrow(PairedData),tmp$SucroseWell]
      }
    }
  }
  PairedData
}

Get.UnPaired.DataFrame<-function(dfms,params,expdesign){
  for(ii in dfms){
    dfm<-DFMClass(ii,p1)
    if(ii == dfms[1]){
      UnPairedData<-data.frame(dfm$LickData$Minutes)  
      names(UnPairedData)<-c("Minutes")
    }
    for(i in 1:6){
      tmp<-Get.Well.And.Treatment(dfm$ID,i,exp.design)
      if(tmp$Treatment2=="Unpaired"){
        colname<-paste(tmp$Treatment2,tmp$Pair,sep="_")
        UnPairedData[[colname]] <- dfm$LickData[1:nrow(UnPairedData),tmp$SucroseWell]
      }
    }
  }
  UnPairedData
}

Convert.Boolean.DataFrame<-function(df){
  df2<-as.matrix(df[,-1])
  df2[df2==TRUE]<-1
  df2[df2==FALSE]<-0
  
  Minutes<-df$Minutes
  new.df<-data.frame(Minutes,df2)
  new.df
}

p1<-ParametersClass.SingleWell()
dfms<-c(1,2,3,4,5,6,7,8)
#dfms<-c(11,12,13,14,15,16,17,18)


exp.design<-read.csv("ExpDesign.csv")
Pair<-rep(1:48,rep(2,48))
exp.design<-data.frame(exp.design,Pair)

PairedData<-Get.Paired.DataFrame(dfms,p1,exp.design)
UnPairedData<-Get.UnPaired.DataFrame(dfms,p1,exp.design)

PairedData<-Convert.Boolean.DataFrame(PairedData)
UnPairedData<-Convert.Boolean.DataFrame(UnPairedData)

write.csv(PairedData,file="PairedRep1.csv",row.names=FALSE)
write.csv(UnPairedData,file="UnPairedRep1.csv",row.names=FALSE)




#write.csv(PairedData,file="PairedRep2.csv",row.names=FALSE)
#write.csv(UnPairedData,file="UnPairedRep2.csv",row.names=FALSE)



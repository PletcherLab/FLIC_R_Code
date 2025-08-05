
breaking.test<-function(dfm,well,lights,end.training=0){
  cname <- paste("W", well, sep = "")
  fdw<-cumsum(dfm$LickData[,cname])
  l<-lights[,c("Minutes",cname)]
  result<-cbind(l,fdw)
  names(result)<-c("Minutes","Lights","CumLicks")
  if(end.training==0){
    end.training<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well==cname]
  }
  ## Get rid of data before/during training
  result<-result[result$Minutes>end.training,]
  #result<-result[result$Lights,]  
  result<-find_transition_rows(result,"Lights")
  DeltaMinutes<-diff(result$Minutes)
  DeltaMinutes<-c(0,DeltaMinutes)
  DeltaLicks<-diff(result$CumLicks)
  DeltaLicks<-c(0,DeltaLicks)
  result<-data.frame(result,DeltaMinutes,DeltaLicks)
  result<-result[result$Lights==TRUE,]
  result
}

##Configuration
## 1 = Lights on wells 1, 5, 9
## 2 = Lights on wells 3, 7, 11
## 3 = Lights on wells 2, 6, 10
## 4 = Lights on wells 4, 8, 12  

breaking.test.dfm<-function(dfm, configuration){
  lights<-GetLightsInfo(dfm)
  result<-list()
  end.training<-rep(NA,12)
  
  if(configuration==1){
    end.training[1:4]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W1"]
    end.training[5:8]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W5"]
    end.training[9:12]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W9"]
  }
  else if(configuration==2){
    end.training[1:4]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W3"]
    end.training[5:8]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W7"]
    end.training[9:12]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W11"]
  }
  else if(configuration==3){
    end.training[1:4]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W2"]
    end.training[5:8]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W6"]
    end.training[9:12]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W10"]
  }
  else if(configuration==4){
    end.training[1:4]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W4"]
    end.training[5:8]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W8"]
    end.training[9:12]<-dfm$InTrainingData$Minutes[dfm$InTrainingData$well=="W12"]
  }
  
  for(i in 1:12){
    cname <- paste("W", i, sep = "")
    tmp<-breaking.test(dfm,i,lights,end.training[i])
    result[[cname]]<-tmp
  }
  result
}

tmp<-breaking.test.dfm(dfm1,1)


for (i in seq_along(tmp)) {
  df <- tmp[[i]]
  name <- names(tmp)[i]
  
  plot(df$Minutes,df$DeltaLicks,ylim=c(0,500))
  title(name)
}


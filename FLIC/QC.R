## These are functions for quality control.


## Give filename to this function and it will do analyses of 
## variance with and without DFM as a factor.  It will also plot
## the data for each well for each treatment in each DFM to look for
## consistency.
QuickAOV.FeedingSummary<-function(datafile="FeedingSummary.csv"){
  data<-read.csv(datafile)
  data$Licks<-data$Licks^0.25
  data$DFM<-factor(data$DFM)
  
  aov.1<-aov(Licks~Treatment,data=data)
  print(summary(aov.1))
  cat("\n********\n")
  aov.2<-aov(Licks~DFM+Treatment,data=data)
  print(summary(aov.2))
  
  g1<-ggplot(data,aes(x=DFM,y=Licks)) + geom_boxplot() + geom_jitter(aes(color=Treatment))
  show(g1)
  
  
  g2<-ggplot(data,aes(x=DFM,y=Licks, fill=Treatment)) + geom_boxplot(aes(fill=Treatment))
  show(g2)
}


CheckForSimultaneousFeeding.DFM<-function(dfm){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-well chambers only")
  lick.matrix<-matrix(rep(NA,24),ncol=4)
  rd<-dfm$LickData[,c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12")]
  rd2<-dfm$BaselineData[,c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12")]
  chamber.sets<-dfm$Parameters$Chamber.Sets
  for(i in 1:6) {
    w1<-chamber.sets[i,1]
    w2<-chamber.sets[i,2]
    l1<-rd[,w1]
    l2<-rd[,w2]
    both<-l1&l2
    
    if(sum(both)>0){
      higher.in.left<-sum(rd2[both,w1]>rd2[both,w2])
      tmp<-rd2[both,]
      tmp2<-tmp[,c(w1,w2)]
      tmp3<-apply(tmp2,1,min)
      lick.matrix[i,]<-c(sum(l1),sum(l2),sum(both),max(tmp3),higher.in.left)
    }
    else {
      lick.matrix[i,]<-c(sum(l1),sum(l2),0,0,0)
    }
  }
  colnames(lick.matrix)<-c("Col1","Col2","Both","MaxMin","HigherInCol1")
  rownames(lick.matrix)<-c("Row1","Row2","Row3","Row4","Row5","Row6")
  
  lick.matrix
}

CheckForBleeding.DFM<-function(dfm,cutoff){
  avg.matrix<-matrix(rep(NA,144),ncol=12)
  for(i in 1:12) {
    rd<-dfm$BaselineData[,c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12")]
    tmp<-rd[rd[,i]>cutoff,]
    averages<-apply(tmp,2,mean)  
    avg.matrix[i,]<-averages
  }
  avg.matrix[is.nan(avg.matrix)]<-0
  result<-list(Matrix = avg.matrix)
  rd<-dfm$BaselineData[,c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12")]
  result$AllData<-averages<-apply(rd,2,mean)  
  rownames(result$Matrix)<-c("W1Sig","W2Sig","W3Sig","W4Sig","W5Sig","W6Sig","W7Sig","W8Sig","W9Sig","W10Sig","W11Sig","W12Sig")
  colnames(result$Matrix)<-c("W1Resp","W2Resp","W3Resp","W4Resp","W5Resp","W6Resp","W7Resp","W8Resp","W9Resp","W10Resp","W11Resp","W12Resp")
  
  ## Now for plotting
  plot.matrix<-avg.matrix
  for(i in 1:12){
    plot.matrix[i,i]<-0
  }
  #filled.contour(t((avg.matrix)),asp=1.0,xlim=c(0,1),ylim=c(0,1), levels=c(0,5,10,15,20,25,30, 500))
  filled.contour(t((plot.matrix)),asp=1.0,xlim=c(0,1),ylim=c(0,1),levels=c(0,5,10,15,20,25,30,500))
  
  result
}

## This function will look for consecutive entries in the
## RawData$Sec column whose difference is larger than it
## should be based on the Samples.Per.Sec parameter.
FindDataBreaks<-function(dfm,multiplier=4,returnvals=TRUE){
  Interval<-diff(dfm$RawData$Seconds)
  Interval<-c(0,Interval)
  thresh<-(1.0/dfm$Parameters$Samples.Per.Second)*multiplier
  Index<-1:length(Interval)
  Index<-Index[Interval>thresh]
  Interval<-Interval[Interval>thresh]
  if(returnvals==TRUE) {
    if(length(Interval)==0)
      c(NA)
    else
      cbind(Index,Interval,dfm$RawData[Index,])  
  }
  else {
    if(length(Interval)==0)
      c(NA)
    else
      c(1)
  } 
  
}

FindDataBreaksV3<-function(dfm,multiplier=4,returnvals=TRUE){
  diffs<-diff(dfm$RawData$Index)
  diffs<-c(0,diffs)
  thresh<-1
  Index<-1:length(diffs)
  Index<-Index[diffs>thresh]
  diffs<-diffs[diffs>thresh]
  if(returnvals==TRUE) {
    if(length(diffs)==0)
      c(NA)
    else
      cbind(Index,diffs,dfm$RawData[Index,])  
  }
  else {
    if(length(diffs)==0)
      c(NA)
    else
      c(1)
  } 
}

## Integrity report for a single DFM object.
## - Reports details of any "error" column(s) present in RawData.
## - Checks whether the Index column increments by exactly one.
## - Prints experiment start/end time and elapsed minutes.
ReportDFMIntegrity<-function(dfm){
  if(is.null(dfm) || !inherits(dfm,"DFM"))
    stop("Argument must be a DFM object.")
  if(is.null(dfm$RawData) || !is.data.frame(dfm$RawData))
    stop("DFM object does not contain a RawData data.frame.")
  
  rd<-dfm$RawData
  n<-nrow(rd)
  max.examples<-25
  
  cat("\n==============================\n")
  cat("DFM Integrity Report\n")
  if(!is.null(dfm$ID)) cat(paste("DFM ID:",dfm$ID,"\n"))
  cat(paste("Rows in RawData:",n,"\n"))
  cat("==============================\n\n")
  
  ## ---- Start/end times and elapsed minutes ----
  start.time<-NA
  end.time<-NA
  elapsed.min<-NA
  elapsed.min.from.minutes<-NA
  
  if(all(c("Date","Time") %in% colnames(rd))){
    tz.use<-"UTC"
    tmp<-as.character(rd$Time[1])
    is.ampm<-FALSE
    if(length(tmp)>0){
      tmp2<-regexpr(".M",tmp)
      is.ampm<-(tmp2[1]>-1)
    }
    if(is.ampm==TRUE){
      fulltimes<-as.POSIXct(paste(rd$Date,rd$Time),format="%m/%d/%Y %I:%M:%S %p",tz=tz.use)
    }
    else {
      fulltimes<-as.POSIXct(paste(rd$Date,rd$Time),format="%m/%d/%Y %H:%M:%S",tz=tz.use)
    }
    if("MSec" %in% colnames(rd)){
      fulltimes<-fulltimes + (as.numeric(rd$MSec)/1000.0)
    }
    if(length(fulltimes)>0 && !all(is.na(fulltimes))){
      start.time<-fulltimes[1]
      end.time<-fulltimes[length(fulltimes)]
      elapsed.min<-as.numeric(difftime(end.time,start.time,units="mins"))
    }
  }
  if("Minutes" %in% colnames(rd) && n>1){
    elapsed.min.from.minutes<-max(rd$Minutes,na.rm=TRUE) - min(rd$Minutes,na.rm=TRUE)
  }
  
  cat("## Experiment timing\n")
  if(!is.na(start.time) && !is.na(end.time)){
    cat(paste("Start time:",format(start.time,usetz=TRUE),"\n"))
    cat(paste("End time:  ",format(end.time,usetz=TRUE),"\n"))
    cat(paste("Elapsed:   ",round(elapsed.min,3)," minutes\n",sep=""))
  }
  else {
    cat("Start/End time: (Date/Time/MSec not found or not parseable)\n")
  }
  if(!is.na(elapsed.min.from.minutes)){
    cat(paste("Elapsed (from Minutes column): ",round(elapsed.min.from.minutes,3)," minutes\n",sep=""))
  }
  cat("\n")
  
  ## ---- Error column(s) details ----
  err.cols<-grep("error",colnames(rd),ignore.case=TRUE,value=TRUE)
  cat("## Error column details\n")
  if(length(err.cols)==0){
    cat("No column with name matching /error/i found in RawData.\n\n")
  }
  else{
    cat(paste("Found error-like column(s):",paste(err.cols,collapse=", "),"\n\n"))
    for(ec in err.cols){
      v<-rd[[ec]]
      nn<-sum(!is.na(v))
      na.count<-sum(is.na(v))
      flagged<-rep(FALSE,length(v))
      if(is.numeric(v) || is.integer(v)){
        flagged<-(!is.na(v) & v!=0)
      }
      else if(is.logical(v)){
        flagged<-(!is.na(v) & v)
      }
      else {
        vv<-as.character(v)
        vv[is.na(vv)]<-""
        flagged<-(vv!="" & vv!="0" & tolower(vv)!="na")
      }
      
      cat(paste("Column:",ec,"\n"))
      cat(paste("  Class:",paste(class(v),collapse=","),"\n"))
      cat(paste("  Non-NA:",nn,"  NA:",na.count,"\n"))
      cat(paste("  Flagged entries:",sum(flagged),"\n"))
      
      ## If this is an encoded error bitmask, decode and summarize.
      ## Bit order (lowest bit first):
      ## 1) I2C, 2) ID, 3) PacketType, 4) DMA_TX, 5) DMA_RX, 6) PacketSize, 7) AIInterrupt, 8) OInterrupt
      error.bit.names<-c(
        "I2C Error",
        "ID Error",
        "PacketType Error",
        "DMA_TX Error",
        "DMA_RX Error",
        "PacketSize Error",
        "AIInterrupt Error",
        "OInterrupt Error"
      )
      decoded.summary.printed<-FALSE
      if(sum(flagged)>0){
        ## Coerce to integer codes if possible
        codes<-v
        if(is.logical(codes)){
          codes<-as.integer(codes)
        }
        else if(!(is.numeric(codes) || is.integer(codes))){
          suppressWarnings(codes<-as.integer(as.character(codes)))
        }
        codes<-as.integer(codes)
        
        if(any(!is.na(codes[flagged]))){
          flagged.idx<-which(flagged & !is.na(codes))
          code.sub<-codes[flagged.idx]
          
          ## Decode bit flags
          bit.mat<-sapply(0:7,function(i){
            bitwAnd(code.sub, bitwShiftL(1,i))>0
          })
          colnames(bit.mat)<-error.bit.names
          
          type.counts<-colSums(bit.mat)
          cat("  Error-type counts (among flagged rows):\n")
          print(type.counts)
          
          ## Also summarize common code values and their decoded meaning
          code.freq<-sort(table(code.sub),decreasing=TRUE)
          code.freq<-head(code.freq,10)
          cat("  Top non-zero error codes (up to 10):\n")
          print(code.freq)
          
          ## Decode the top codes into names (for readability)
          top.codes<-as.integer(names(code.freq))
          decode.one<-function(code){
            if(is.na(code)) return(NA)
            bits<-sapply(0:7,function(i) bitwAnd(code,bitwShiftL(1,i))>0)
            names(bits)<-error.bit.names
            if(sum(bits)==0) return("")
            paste(names(bits)[bits],collapse="; ")
          }
          decoded<-sapply(top.codes,decode.one)
          decode.map<-data.frame(Code=top.codes,Decoded=as.character(decoded))
          cat("  Decoded top codes:\n")
          print(decode.map,row.names=FALSE)
          decoded.summary.printed<-TRUE
        }
      }
      
      ## Value summary
      if((is.numeric(v) || is.integer(v) || is.logical(v)) && nn>0 && decoded.summary.printed==FALSE){
        tv<-sort(table(v, useNA="ifany"),decreasing=TRUE)
        tv<-head(tv,10)
        cat("  Top values (up to 10):\n")
        print(tv)
      }
      else if(nn>0){
        tv<-sort(table(as.character(v), useNA="ifany"),decreasing=TRUE)
        tv<-head(tv,10)
        cat("  Top values (up to 10):\n")
        print(tv)
      }
      
      ## Example rows where flagged
      if(sum(flagged)>0){
        idx<-which(flagged)
        idx<-head(idx,max.examples)
        show.cols<-c()
        for(x in c("Minutes","Seconds","Index","Date","Time","MSec")){
          if(x %in% colnames(rd)) show.cols<-c(show.cols,x)
        }
        show.cols<-unique(c("Row",show.cols,ec))
        tmp<-data.frame(Row=idx, rd[idx, intersect(colnames(rd), show.cols[show.cols!="Row"]), drop=FALSE])
        if(!(ec %in% colnames(tmp))) tmp[[ec]]<-v[idx]
        
        ## Add decoded types if numeric/integer-like
        codes.example<-v[idx]
        if(is.logical(codes.example)){
          codes.example<-as.integer(codes.example)
        }
        else if(!(is.numeric(codes.example) || is.integer(codes.example))){
          suppressWarnings(codes.example<-as.integer(as.character(codes.example)))
        }
        codes.example<-as.integer(codes.example)
        if(any(!is.na(codes.example))){
          decode.one<-function(code){
            if(is.na(code) || code==0) return("")
            bits<-sapply(0:7,function(i) bitwAnd(code,bitwShiftL(1,i))>0)
            names(bits)<-error.bit.names
            if(sum(bits)==0) return("")
            paste(names(bits)[bits],collapse="; ")
          }
          tmp$ErrorTypes<-sapply(codes.example,decode.one)
        }
        cat(paste("  First",nrow(tmp),"flagged row(s):\n"))
        print(tmp)
      }
      cat("\n")
    }
  }
  
  ## ---- Index increment-by-one check ----
  cat("## Index increment check\n")
  if(!("Index" %in% colnames(rd))){
    cat("No `Index` column found in RawData.\n")
    cat("Skipping increment-by-one analysis.\n\n")
  }
  else{
    ix<-rd$Index
    if(!is.numeric(ix) && !is.integer(ix)){
      cat(paste("Index column exists but is type:",paste(class(ix),collapse=","),"\n"))
      cat("Skipping increment-by-one analysis.\n\n")
    }
    else if(length(ix)<2){
      cat("Index column has <2 entries; cannot evaluate increments.\n\n")
    }
    else{
      d<-diff(ix)
      ok<-all(d==1,na.rm=TRUE)
      if(ok==TRUE){
        cat("Index increments by exactly 1 for all consecutive rows.\n\n")
      }
      else{
        cat("Index does NOT always increment by 1.\n")
        
        ## Jumps forward (missing indices)
        jumps<-which(d>1)
        if(length(jumps)>0){
          cat(paste("Forward jumps (missing indices):",length(jumps),"\n"))
          miss<-d[jumps]-1
          start.idx<-ix[jumps]
          end.idx<-ix[jumps+1]
          row.at<-jumps+1
          
          mins<-rep(NA,length(row.at))
          secs<-rep(NA,length(row.at))
          if("Minutes" %in% colnames(rd)) mins<-rd$Minutes[row.at]
          if("Seconds" %in% colnames(rd)) secs<-rd$Seconds[row.at]
          
          report<-data.frame(Row=row.at,
                             Minutes=mins,
                             Seconds=secs,
                             IndexBefore=start.idx,
                             IndexAfter=end.idx,
                             MissingIndices=miss)
          cat(paste("Total missing indices across all jumps:",sum(miss),"\n"))
          print(report)
          cat("\n")
        }
        else{
          cat("No forward jumps (>1) detected.\n\n")
        }
        
        ## Non-increasing / duplicates
        bad<-which(d<=0 | (d!=1 & d<=1))
        bad<-bad[!bad %in% jumps]
        if(length(bad)>0){
          cat(paste("Non-increment anomalies (duplicates or decreasing):",length(bad),"\n"))
          row.at<-bad+1
          mins<-rep(NA,length(row.at))
          secs<-rep(NA,length(row.at))
          if("Minutes" %in% colnames(rd)) mins<-rd$Minutes[row.at]
          if("Seconds" %in% colnames(rd)) secs<-rd$Seconds[row.at]
          report2<-data.frame(Row=row.at,
                              Minutes=mins,
                              Seconds=secs,
                              IndexBefore=ix[bad],
                              IndexAfter=ix[bad+1],
                              Delta=d[bad])
          print(report2)
          cat("\n")
        }
      }
    }
  }
  
  invisible(list(
    dfm_id=dfm$ID,
    n_rawdata=n,
    start_time=start.time,
    end_time=end.time,
    elapsed_minutes=elapsed.min,
    elapsed_minutes_from_minutes_col=elapsed.min.from.minutes,
    error_columns=err.cols
  ))
}

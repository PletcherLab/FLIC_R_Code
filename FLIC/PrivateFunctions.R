

##### Basic calculation functions #####
## This function takes a vector of dates (as strings), a vector
## of times (24 hour as string) and a parameters object.
## it returns the elapsed seconds.
GetElapsedSeconds <- function(dfm) {
  dates <- dfm$Date
  times <- dfm$Time
  ms <- dfm$MSec
  ## For US culture.
  tmp <- as.character(dfm$Time[1])
  tmp2 <- regexpr('.M', tmp)
  if (tmp2[1] > -1) {
    ## There seems to be some confusion about the nature of the time stamp
    ## from different MCU.
    ## Use this one if time in AM/PM
    fulltimes <- as.POSIXct(paste(dates, times), format = "%m/%d/%Y %I:%M:%S %p", tz =
                              'UTC')
  }
  else {
    ## Use this one if time is military time.
    fulltimes <- as.POSIXct(paste(dates, times), format = "%m/%d/%Y %H:%M:%S", tz =
                              'UTC')
  }
  diffs <- as.numeric(c(difftime(fulltimes, fulltimes[1], units = "secs")))
  diffs <- diffs + (ms / 1000)
  diffs
}

CalculateProgressiveRatioTraining <- function(dfm) {
  newData <- dfm$RawData
  # the number of samples in those minutes
  
  inTrainingData <- newData[, 1:18]
  ## Value to subtract is 8388608
  ## or because I think the MCU divides all reported
  ## numb by 128, the number to subtract should be
  ## 65536
  for (i in 1:12) {
    cname <- paste("W", i, sep = "")
    intraining <- newData[, cname] > 40000
    inTrainingData[, cname] <- intraining
    newData[intraining, cname] <- newData[intraining, cname] - 65536
  }
  
  dfm$RawData = newData
  dfm$InTrainingData = GetDoneTrainingInfo(inTrainingData)
  dfm
}

GetDoneTrainingInfo <- function(data) {
  # the number of samples in those minutes
  
  results <- data.frame(names(data)[7:18], matrix(rep(NA, 12 * 2), ncol =
                                                    2))
  names(results) <- c("well", "Minutes", "Sample")
  
  for (i in 1:12) {
    cname <- paste("W", i, sep = "")
    intrainingMin <- data$Minutes[data[, cname]]
    intrainingSamp <- data$Sample[data[, cname]]
    if (length(intrainingMin) != 0) {
      results[i, 2] <- max(intrainingMin)
      results[i, 3] <- max(intrainingSamp)
    }
  }
  results
}


CalculateBaseline = function(dfm) {
  window.min = dfm$Parameters$Baseline.Window.Minutes
  newData <- dfm$RawData
  # the number of samples in those minutes
  window <- window.min * 60 * 5
  if (window %% 2 == 0)
    window = window + 1
  
  for (i in 1:12) {
    cname <- paste("W", i, sep = "")
    tmp <- runmed(newData[, cname], window)
    newData[, cname] <- newData[, cname] - tmp
  }
  
  dfm$BaselineData = newData
  ## Everything else must be recalculated
  dfm <- SetThreshold(dfm)
  dfm
}


SetThreshold = function(dfm, getStandard = TRUE) {
  ## First set the threshold...
  if (is.null(dfm$BaselineData)) {
    stop("DFM must have baseline.")
  }
  
  dfm <- Set.Fixed.Threshold(dfm)
  
  ## Now update the licks and PI
  dfm <- Set.Feeding.Data(dfm)
  dfm <- Set.Tasting.Data(dfm)
  
  #Other measures
  dfm <- Set.Durations.And.Intervals(dfm)
  dfm <- Set.Tasting.Durations.And.Intervals(dfm)
  dfm
  
}

Set.Feeding.Data <- function(dfm) {
  if (is.null(dfm$BaselineData))
    stop("Baseline must be calculated")
  
  newData <- dfm$BaselineData
  newData2 <- dfm$BaselineData
  
  for (i in 1:12) {
    tmp <- Set.Feeding.Data.Well(dfm, i)
    cname <- paste("W", i, sep = "")
    newData[, cname] <- tmp[, 1]
    newData2[, cname] <- tmp[, 2]
  }
  
  dfm$LickData <- newData
  dfm$EventData <- newData2
  
  
  ## Can only adjust after getting original licks
  ## to find overlap, then correct and recalculate
  if (dfm$Parameters$Chamber.Size == 2) {
    dfm$LickMatrix <- CheckForSimultaneousFeeding.DFM(dfm)
    
    if (dfm$Parameters$Correct.For.Dual.Feeding == TRUE) {
      stored.baseline.data <- dfm$BaselineData
      dfm <- Adjust.Baseline.For.Dual.Feeding.DFM(dfm)
      for (i in 1:12) {
        tmp <- Set.Feeding.Data.Well(dfm, i)
        cname <- paste("W", i, sep = "")
        newData[, cname] <- tmp[, 1]
        newData2[, cname] <- tmp[, 2]
      }
      
      dfm$BaselineData <- stored.baseline.data
      dfm$LickData <- newData
      dfm$EventData <- newData2
    }
  }
  
  dfm
}



Adjust.Baseline.For.Dual.Feeding.DFM <- function(dfm) {
  if (dfm$Parameters$Chamber.Size != 2)
    stop("This function is for two-well chambers only")
  rd <- dfm$LickData[, c("W1",
                         "W2",
                         "W3",
                         "W4",
                         "W5",
                         "W6",
                         "W7",
                         "W8",
                         "W9",
                         "W10",
                         "W11",
                         "W12")]
  rd2 <- dfm$BaselineData[, c("W1",
                              "W2",
                              "W3",
                              "W4",
                              "W5",
                              "W6",
                              "W7",
                              "W8",
                              "W9",
                              "W10",
                              "W11",
                              "W12")]
  chamber.sets <- dfm$Parameters$Chamber.Sets
  Chamber <- c(NA)
  dual.feeding.data <- data.frame(Chamber, dfm$BaselineData[1, ])
  for (i in 1:6) {
    w1 <- chamber.sets[i, 1]
    w2 <- chamber.sets[i, 2]
    l1 <- rd[, w1]
    l2 <- rd[, w2]
    both <- l1 & l2
    
    if (sum(both) > 0) {
      cat(
        paste(
          "Dual feeding signal detected in DFM",
          dfm$ID,
          " Chamber",
          i,
          ". QC for feeding and bleeding!\n",
          sep = ""
        )
      )
      l1 <- rd2[, w1]
      l2 <- rd2[, w2]
      
      smaller.1 <- both & (l1 < l2)
      smaller.2 <- both & (l2 < l1)
      same <- both & (l2 == l1)
      
      rd2[smaller.1, w1] <- 0
      rd2[smaller.2, w2] <- 0
      rd2[same, w1] <- 0
      rd2[same, w2] <- 0
      
      tmp <- dfm$BaselineData[both, ]
      Chamber <- rep(i, nrow(tmp))
      tmp2 <- data.frame(Chamber, tmp)
      
      ## Now append to dual.feeding.data
      dual.feeding.data <- rbind(dual.feeding.data, tmp2)
    }
    
  }
  dual.feeding.data <- dual.feeding.data[-1, ]
  dfm$DualFeedingData <- dual.feeding.data
  dfm$BaselineData[, c("W1",
                       "W2",
                       "W3",
                       "W4",
                       "W5",
                       "W6",
                       "W7",
                       "W8",
                       "W9",
                       "W10",
                       "W11",
                       "W12")] <- rd2
  dfm
}


Set.Feeding.Data.Well <- function(dfm, well) {
  ## Get all possible feeding Licks
  thresh <- Thresholds.Well(dfm, well)
  data <- BaselinedData.Well(dfm, well)
  
  Feeding.Licks.Min <- (data > thresh$FeedingMin)
  
  Feeding.Licks.Max <- (data > thresh$FeedingMax)
  
  ## Find continguous events above min threshold with at least one value above max threshold.
  ## The result of this function is also equivalent to the Events vector
  Events <- Get.Surviving.Events(Feeding.Licks.Min, Feeding.Licks.Max)
  
  ## Now remove events that are too short
  Events[Events < dfm$Parameters$Feeding.Minevents] <- 0
  
  ## Now expand the licks to TRUE/FALSE entries
  FeedingLicks <- Expand.Events(Events)
  
  ## Now Bridge sporadic lick events into single events.
  tmp <- Link.Events(FeedingLicks, dfm$Parameters$Feeding.Event.Link.Gap)
  Events <- Get.Events(tmp)
  
  data.frame(FeedingLicks, Events)
}


Set.Tasting.Data <- function(dfm) {
  if (is.null(dfm$BaselineData))
    stop("Baseline must be calculated")
  if (is.null(dfm$LickData))
    stop("Feeding Licks must be calculated")
  
  newData <- dfm$BaselineData
  newData2 <- dfm$BaselineData
  
  for (i in 1:12) {
    tmp <- Set.Tasting.Data.Well(dfm, i)
    cname <- paste("W", i, sep = "")
    newData[, cname] <- tmp[, 1]
    newData2[, cname] <- tmp[, 2]
  }
  dfm$TastingData <- newData
  dfm$TastingEventData <- newData2
  dfm
}
Set.Tasting.Data.Well <- function(dfm, well) {
  ## Get Tasting Licks
  ## Note that Feeding Licks have to be calculated first because if the fly is
  ## feeding, then tasting events have to be cancelled.
  thresh <- Thresholds.Well(dfm, well)
  data <- BaselinedData.Well(dfm, well)
  
  Licks <- (data > thresh$TastingMin &
              data < thresh$TastingMax)
  
  FeedingLicks <- FeedingData.Well.Licks(dfm, well)
  
  ## Keep only taste licks that are not feeding licks
  Licks[FeedingLicks] <- FALSE
  
  Events <- Get.Events(Licks)
  
  ## Now remove events that are too short
  Events[Events < dfm$Parameters$Tasting.Minevents] <- 0
  
  data.frame(Licks, Events)
}
Set.Fixed.Threshold <- function(dfm) {
  tmp <- Set.Fixed.Threshold.Well(dfm, 1)
  Thresholds = list(W1 = tmp)
  
  for (i in 2:12) {
    s <- paste("W", i, sep = "")
    tmp <- Set.Fixed.Threshold.Well(dfm, i)
    Thresholds[[s]] <- tmp
  }
  dfm$Thresholds <- Thresholds
  dfm
}
Set.Fixed.Threshold.Well <- function(dfm, well) {
  n <- SampleCount(dfm)
  ## Get well specific thresholds if the values are < 0
  if (dfm$Parameters$Feeding.Threshold < 0) {
    ## Find maximum reading
    tmp <- max(BaselinedData.Well(dfm, well))
    tmpA <- round(tmp * abs(dfm$Parameters$Feeding.Threshold), 0)
    tmpB <- round(tmp * abs(dfm$Parameters$Feeding.Minimum), 0)
    tmpC <- round(tmp * abs(dfm$Parameters$Tasting.Minimum), 0)
    tmpD <- round(tmp * abs(dfm$Parameters$Tasting.Maximum), 0)
  }
  else {
    tmpA <- dfm$Parameters$Feeding.Threshold
    tmpB <- dfm$Parameters$Feeding.Minimum
    tmpC <- dfm$Parameters$Tasting.Minimum
    tmpD <- dfm$Parameters$Tasting.Maximum
  }
  
  feeding.max.thresh <- rep(tmpA, n)
  feeding.min.thresh <- rep(tmpB, n)
  tasting.min.thresh <- rep(tmpC, n)
  tasting.max.thresh <- rep(tmpD, n)
  
  
  r.tmp <- data.frame(feeding.max.thresh,
                      feeding.min.thresh,
                      tasting.max.thresh,
                      tasting.min.thresh)
  
  names(r.tmp) <- c("FeedingMax", "FeedingMin", "TastingMax", "TastingMin")
  r.tmp
}
Set.Durations.And.Intervals <- function(dfm) {
  tmp <- Set.Durations.And.Intervals.Well(dfm, 1)
  Durations = list(W1 = tmp$Durations)
  Intervals = list(W1 = tmp$Intervals)
  
  for (i in 2:12) {
    s <- paste("W", i, sep = "")
    tmp <- Set.Durations.And.Intervals.Well(dfm, i)
    Durations[[s]] <- tmp$Durations
    Intervals[[s]] <- tmp$Intervals
  }
  
  dfm$Durations <- Durations
  dfm$Intervals <- Intervals
  dfm
}
Set.Durations.And.Intervals.Well <- function(dfm, well) {
  data <- BaselineData.Well(dfm, well)
  events <- FeedingData.Well.Events(dfm, well)
  ## Now we need to update the event durations
  ## Indices will be used for summary duration characteristics
  indices <- 1:length(events)
  
  indices <- indices[events > 0]
  boutDurs <- events[events > 0]
  
  Durations <- 0
  
  if (length(boutDurs) > 0) {
    max.inten <- rep(0, length(indices))
    min.inten <- rep(0, length(indices))
    sum.inten <- rep(0, length(indices))
    avg.inten <- rep(0, length(indices))
    var.inten <- rep(0, length(indices))
    for (i in 1:length(indices)) {
      dataindex <- indices[i]
      eventlength <- boutDurs[i]
      tmp2 <- data[dataindex:(dataindex + (eventlength - 1))]
      max.inten[i] <- max(tmp2)
      min.inten[i] <- min(tmp2)
      sum.inten[i] <- sum(tmp2)
      avg.inten[i] <- mean(tmp2)
      var.inten[i] <- var(tmp2)
    }
    
    BoutData <- data.frame(min.inten, max.inten, sum.inten, avg.inten, var.inten)
    names(BoutData) <- c(
      "MinIntensity",
      "MaxIntensity",
      "SumIntensity",
      "MeanIntensity",
      "VarIntensity"
    )
    
    tmp <- BaselineData(dfm)
    tmp <- tmp[indices, ]
    Minutes <- tmp$Minutes
    Events <- boutDurs
    Duration <- Events / dfm$Parameters$Samples.Per.Sec
    AvgInten <- BoutData$MeanIntensity
    MaxInten <- BoutData$MaxIntensity
    MinInten <- BoutData$MinIntensity
    SumInten <- BoutData$SumIntensity
    VarInten <- BoutData$VarIntensity
    Durations <- data.frame(Minutes,
                            Events,
                            Duration,
                            SumInten,
                            AvgInten,
                            MinInten,
                            MaxInten,
                            VarInten)
    names(Durations) <- c(
      "Minutes",
      "Licks",
      "Duration",
      "TotalIntensity",
      "AvgIntensity",
      "MinIntensity",
      "MaxIntensity",
      "VarIntensity"
    )
  }
  
  result <- list(Durations = Durations)
  
  ## Now intervals
  
  ## Collapse feeding data to time BETWEEN events.
  ##boutInt<-Get.Intervals(FeedingData.Well.Licks(dfm,well))
  tmp <- FeedingData.Well.Events(dfm, well)
  tmp <- Expand.Events(tmp)
  boutInt <- Get.Intervals(tmp)
  
  
  indices <- 1:length(boutInt)
  indices <- indices[boutInt > 0]
  boutInt <- boutInt[boutInt > 0]
  
  
  spm <- dfm$Parameters$Samples.Per.Sec
  intA <- boutInt / spm
  
  Ints <- 0
  
  if (length(intA) > 0) {
    tmp <- BaselineData(dfm)
    tmp <- tmp[indices, ]
    Minutes <- tmp$Minutes
    Sample <- tmp$Sample
    IntervalSec <- intA
    Ints <- data.frame(Minutes, Sample, IntervalSec)
  }
  
  result <- list(Durations = Durations, Intervals = Ints)
  result
}
Set.Tasting.Durations.And.Intervals <- function(dfm) {
  tmp <- Set.Tasting.Durations.And.Intervals.Well(dfm, 1)
  Durations = list(W1 = tmp$Durations)
  Intervals = list(W1 = tmp$Intervals)
  
  for (i in 2:12) {
    s <- paste("W", i, sep = "")
    tmp <- Set.Tasting.Durations.And.Intervals.Well(dfm, i)
    Durations[[s]] <- tmp$Durations
    Intervals[[s]] <- tmp$Intervals
  }
  
  dfm$TastingDurations <- Durations
  dfm$TastingIntervals <- Intervals
  dfm
}
Set.Tasting.Durations.And.Intervals.Well <- function(dfm, well) {
  data <- BaselineData.Well(dfm, well)
  events <- TastingData.Well.Events(dfm, well)
  ## Now we need to update the event durations
  ## Indices will be used for summary duration characteristics
  indices <- 1:length(events)
  
  indices <- indices[events > 0]
  boutDurs <- events[events > 0]
  
  Durations <- 0
  
  if (length(boutDurs) > 0) {
    max.inten <- rep(0, length(indices))
    min.inten <- rep(0, length(indices))
    sum.inten <- rep(0, length(indices))
    avg.inten <- rep(0, length(indices))
    var.inten <- rep(0, length(indices))
    for (i in 1:length(indices)) {
      dataindex <- indices[i]
      eventlength <- boutDurs[i]
      tmp2 <- data[dataindex:(dataindex + (eventlength - 1))]
      max.inten[i] <- max(tmp2)
      min.inten[i] <- min(tmp2)
      sum.inten[i] <- sum(tmp2)
      avg.inten[i] <- mean(tmp2)
      var.inten[i] <- var(tmp2)
    }
    
    BoutData <- data.frame(min.inten, max.inten, sum.inten, avg.inten, var.inten)
    names(BoutData) <- c(
      "MinIntensity",
      "MaxIntensity",
      "SumIntensity",
      "MeanIntensity",
      "VarIntensity"
    )
    
    tmp <- BaselineData(dfm)
    tmp <- tmp[indices, ]
    Minutes <- tmp$Minutes
    Events <- boutDurs
    Duration <- Events / dfm$Parameters$Samples.Per.Sec
    AvgInten <- BoutData$MeanIntensity
    MaxInten <- BoutData$MaxIntensity
    MinInten <- BoutData$MinIntensity
    SumInten <- BoutData$SumIntensity
    VarInten <- BoutData$VarIntensity
    Durations <- data.frame(Minutes,
                            Events,
                            Duration,
                            SumInten,
                            AvgInten,
                            MinInten,
                            MaxInten,
                            VarInten)
    names(Durations) <- c(
      "Minutes",
      "Licks",
      "Duration",
      "TotalIntensity",
      "AvgIntensity",
      "MinIntensity",
      "MaxIntensity",
      "VarIntensity"
    )
  }
  
  result <- list(Durations = Durations)
  
  ## Now intervals
  
  ## Collapse feeding data to time BETWEEN events.
  boutInt <- Get.Intervals(FeedingData.Well.Licks(dfm, well))
  
  indices <- 1:length(boutInt)
  indices <- indices[boutInt > 0]
  boutInt <- boutInt[boutInt > 0]
  
  
  spm <- dfm$Parameters$Samples.Per.Sec
  intA <- boutInt / spm
  
  Ints <- 0
  
  if (length(intA) > 0) {
    tmp <- BaselineData(dfm)
    tmp <- tmp[indices, ]
    Minutes <- tmp$Minutes
    Sample <- tmp$Sample
    IntervalSec <- intA
    Ints <- data.frame(Minutes, Sample, IntervalSec)
  }
  
  result <- list(Durations = Durations, Intervals = Ints)
  result
}
Thresholds.Well <- function(dfm, well, range = c(0, 0)) {
  cname = paste("W", well, sep = "")
  tmp <- dfm$Thresholds[[cname]]
  if (sum(range) != 0) {
    tmp <- tmp[(dfm$BaselineData$Minutes > range[1]) &
                 (dfm$BaselineData$Minutes <= range[2]), ]
  }
  tmp
}

##### Data Access functions #####
BaselinedData.Well <- function(dfm, well, range = c(0, 0)) {
  cname = paste("W", well, sep = "")
  tmp <- dfm$BaselineData[, cname]
  if (sum(range) != 0) {
    tmp <- tmp[(dfm$BaselineData$Minutes > range[1]) &
                 (dfm$BaselineData$Minutes <= range[2])]
  }
  tmp
}
BaselinedData <- function(dfm, range = c(0, 0)) {
  tmp <- dfm$BaselineData
  if (sum(range) != 0) {
    tmp <- tmp[(dfm$BaselineData$Minutes > range[1]) &
                 (dfm$BaselineData$Minutes <= range[2]), ]
  }
  tmp
}
SampleCount <- function(dfm, range = c(0, 0)) {
  nrow(BaselinedData(dfm, range))
}
FeedingData.Well.Licks <- function(dfm, well, range = c(0, 0)) {
  cname = paste("W", well, sep = "")
  tmp <- FeedingData.Licks(dfm, range)
  tmp[, cname]
}
## Remember that this function returns a vector with
## duration of event information as well.
## Need to set these to 1 to get number of events.
FeedingData.Well.Events <- function(dfm, well, range = c(0, 0)) {
  cname = paste("W", well, sep = "")
  tmp <- FeedingData.Events(dfm, range)
  tmp[, cname]
}
TastingData.Well <- function(dfm, well, range = c(0, 0)) {
  cname = paste("W", well, sep = "")
  tmp <- dfm$TastingData[, cname]
  if (sum(range) != 0) {
    tmp <- tmp[(tmp$Minutes > range[1]) & (tmp$Minutes <= range[2])]
  }
  tmp
}
TastingData.Well.Events <- function(dfm, well, range = c(0, 0)) {
  cname = paste("W", well, sep = "")
  tmp <- TastingData.Events(dfm, range)
  tmp[, cname]
}
TastingData.Events <- function(dfm, range = c(0, 0)) {
  data <- dfm$TastingEventData
  if (sum(range) != 0) {
    data <- data[(data$Minutes > range[1] & data$Minutes <= range[2]), ]
  }
  data
}
FeedingData.Licks <- function(dfm, range = c(0, 0)) {
  data <- dfm$LickData
  if (sum(range) != 0) {
    data <- data[(data$Minutes > range[1] & data$Minutes <= range[2]), ]
  }
  data
}
## Remember that this function returns a vector with
## duration of event information as well.
## Need to set these to 1 to get number of events.
FeedingData.Events <- function(dfm, range = c(0, 0)) {
  data <- dfm$EventData
  if (sum(range) != 0) {
    data <- data[(data$Minutes > range[1] & data$Minutes <= range[2]), ]
  }
  data
}
TastingData <- function(dfm, range = c(0, 0)) {
  data <- dfm$TastingData
  if (sum(range) != 0) {
    data <- data[(data$Minutes > range[1] & data$Minutes <= range[2]), ]
  }
  data
}
Feeding.TotalLicks <- function(dfm, range = c(0, 0)) {
  result <- rep(-1, 12)
  data <- FeedingData.Licks(dfm, range)
  for (i in 1:12) {
    cname = paste("W", i, sep = "")
    tmp <- data[, cname]
    result[i] <- sum(tmp)
  }
  names(result) <- paste("W", 1:12, sep = "")
  result
}
Feeding.TotalLicks.Well <- function(dfm, well, range = c(0, 0)) {
  tmp <- Feeding.TotalLicks(dfm, range)
  tmp[well]
}
Feeding.TotalEvents <- function(dfm, range = c(0, 0)) {
  result <- rep(-1, 12)
  data <- FeedingData.Events(dfm, range)
  for (i in 1:12) {
    cname = paste("W", i, sep = "")
    tmp <- data[, cname]
    result[i] <- sum(tmp > 0)
  }
  names(result) <- paste("W", 1:12, sep = "")
  result
}
Feeding.TotalEvents.Well <- function(dfm, well, range = c(0, 0)) {
  tmp <- Feeding.TotalEvents(dfm, range)
  tmp[well]
}
Tasting.TotalLicks <- function(dfm, range = c(0, 0)) {
  result <- rep(-1, 12)
  data <- TastingData(dfm, range)
  for (i in 1:12) {
    cname = paste("W", i, sep = "")
    tmp <- data[, cname]
    result[i] <- sum(tmp)
  }
  names(result) <- paste("W", 1:12, sep = "")
  result
}
Tasting.TotalLicks.Well <- function(dfm, well, range = c(0, 0)) {
  tmp <- Tasting.TotalLicks(dfm, range)
  tmp[well]
}
BaselineData.Well = function(dfm, well, range = c(0, 0)) {
  cname = paste("W", well, sep = "")
  tmp <- BaselineData(dfm, range)
  tmp[, cname]
}
BaselinedData.Range.Well <- function(dfm, well, range = c(0, 0)) {
  tmp <- BaselinedData.Well(dfm, well, range)
  x1 <- min(tmp)
  x2 <- max(tmp)
  c(x1, x2)
}
Minutes <- function(dfm, range = c(0, 0)) {
  data <- dfm$BaselineData$Minutes
  if (sum(range) != 0) {
    data <- data[(data > range[1] & data < range[2])]
  }
  data
}
Feeding.Durations.Well <- function(dfm, well) {
  cname = paste("W", well, sep = "")
  adurs <- dfm$Durations[[cname]]
  adurs
}
Feeding.Intervals.Well <- function(dfm, well) {
  cname = paste("W", well, sep = "")
  adurs <- dfm$Intervals[[cname]]
  adurs
}
LastSampleData.Well <- function(dfm, well) {
  tmp <- BaselinedData.Well(dfm, well)
  tmp[length(tmp)]
}
FirstSampleData.Well <- function(dfm, well) {
  tmp <- BaselinedData.Well(dfm, well)
  tmp[1]
}
## These functions will output the intervals
GetIntervalData.Well <- function(dfm, well, range = c(0, 0)) {
  nameA <- paste("W", well, sep = "")
  parameter.vector <- GetDFMParameterVector(dfm)
  pnames <- Get.Parameter.Names(dfm$Parameters)
  
  theData <- dfm$Intervals[[nameA]]
  if (length(theData) == 1 && theData[1] == 0) {
    theData <- data.frame(matrix(rep(NA, 8), nrow = 1))
  }
  else {
    if (sum(range) != 0) {
      theData <- theData[(theData$Minutes > range[1] &
                            theData$Minutes <= range[2]), ]
    }
  }
  
  Well <- rep(well, nrow(theData))
  chamber <- rep(GetChamberFromWell(dfm, well), nrow(theData))
  TCWell <- rep(GetTCWellFromWell(dfm, well), nrow(theData))
  DFM <- rep(dfm$ID, nrow(theData))
  
  tmpA <- data.frame(DFM, chamber, TCWell, Well, theData)
  tmp2 <- matrix(
    rep(parameter.vector, nrow(tmpA)),
    ncol = length(parameter.vector),
    byrow = TRUE
  )
  tmp3 <- data.frame(tmpA, tmp2)
  names(tmp3) <- c("DFM",
                   "Chamber",
                   "TCWell",
                   "Well",
                   "Minutes",
                   "Sample",
                   "IntervalSec",
                   pnames)
  
  if (dfm$Parameters$Chamber.Size == 1)
    tmp3 <- tmp3[, !names(tmp3) %in% "TCWell"]
  
  tmp3
}
GetDurationData.Well <- function(dfm, well, range = c(0, 0)) {
  nameA <- paste("W", well, sep = "")
  parameter.vector <- GetDFMParameterVector(dfm)
  pnames <- Get.Parameter.Names(dfm$Parameters)
  
  theData <- dfm$Durations[[nameA]]
  if (length(theData) == 1 && theData[1] == 0) {
    theData <- data.frame(matrix(rep(NA, 8), nrow = 1))
  }
  else {
    if (sum(range) != 0) {
      theData <- theData[(theData$Minutes > range[1] &
                            theData$Minutes <= range[2]), ]
    }
    if (nrow(theData) == 0) {
      theData <- data.frame(matrix(rep(NA, 8), nrow = 1))
    }
  }
  
  
  
  Well <- rep(well, nrow(theData))
  chamber <- rep(GetChamberFromWell(dfm, well), nrow(theData))
  TCWell <- rep(GetTCWellFromWell(dfm, well), nrow(theData))
  DFM <- rep(dfm$ID, nrow(theData))
  
  tmpA <- data.frame(DFM, chamber, TCWell, Well, theData)
  tmp2 <- matrix(
    rep(parameter.vector, nrow(tmpA)),
    ncol = length(parameter.vector),
    byrow = TRUE
  )
  tmp3 <- data.frame(tmpA, tmp2)
  names(tmp3) <- c(
    "DFM",
    "Chamber",
    "TCWell",
    "Well",
    "Minutes",
    "Licks",
    "Duration",
    "TotalIntensity",
    "AvgIntensity",
    "MinIntensity",
    "MaxIntensity",
    "VarIntensity",
    pnames
  )
  
  if (dfm$Parameters$Chamber.Size == 1)
    tmp3 <- tmp3[, !names(tmp3) %in% "TCWell"]
  
  tmp3
}
GetTCWellFromWell <- function(dfm, well) {
  if (dfm$Parameters$Chamber.Size == 1) {
    well <- NA
  }
  else {
    if (well == 1 ||
        well == 3 || well == 5 || well == 7 || well == 9 || well == 11) {
      if (dfm$Parameters$PI.Multiplier == 1) {
        well <- "WellA"
      }
      else {
        well <- "WellB"
      }
    }
    
    else {
      if (dfm$Parameters$PI.Multiplier == 1) {
        well <- "WellB"
      }
      else {
        well <- "WellA"
      }
    }
  }
  well
}
GetChamberFromWell <- function(dfm, well) {
  if (dfm$Parameters$Chamber.Size == 1) {
    chamber <- well
  }
  else {
    if (well == 1 || well == 2)
      chamber <- 1
    else if (well == 3 || well == 4)
      chamber <- 2
    else if (well == 5 || well == 6)
      chamber <- 3
    else if (well == 7 || well == 8)
      chamber <- 4
    else if (well == 9 || well == 10)
      chamber <- 5
    else if (well == 11 || well == 12)
      chamber <- 6
  }
  chamber
}
UpdateHiddenDFMObject <- function(dfm) {
  st <- paste("DFM", dfm$ID, sep = "")
  assign(st, dfm, pos = 1)
}
GetDFMParameterVector <- function(dfm) {
  GetParameterVector(dfm$Parameters)
}


##### Analysis helper functions #####
Feeding.IntervalSummary.Well <- function(dfm, well, range = c(0, 0)) {
  cname = paste("W", well, sep = "")
  adurs <- dfm$Intervals[[cname]]
  if (sum(range) != 0) {
    if (!is.data.frame(adurs)) {
      a <- 0
      aa <- 0
    }
    else {
      adurs <- adurs[(adurs$Minutes > range[1]) &
                       (adurs$Minutes <= range[2]), ]
      if (nrow(adurs) == 0) {
        a <- 0
        aa <- 0
      }
      else {
        a <- mean(adurs$IntervalSec)
        aa <- median(adurs$IntervalSec)
      }
    }
  }
  else {
    if (!is.data.frame(adurs)) {
      a <- 0
      aa <- 0
    } else {
      a <- mean(adurs$IntervalSec)
      aa <- median(adurs$IntervalSec)
    }
  }
  
  if (is.na(a) || is.nan(a))
    a <- 0
  if (is.na(aa) || is.nan(aa))
    aa <- 0
  tmp <- data.frame(a, aa)
  names(tmp) <- c("MeanTimeBtw", "MedTimeBtw")
  tmp
}
Feeding.DurationSummary.Well <- function(dfm, well, range = c(0, 0)) {
  cname = paste("W", well, sep = "")
  adurs <- dfm$Durations[[cname]]
  if (sum(range) != 0) {
    if (!is.data.frame(adurs)) {
      a <- NA
      aa <- NA
    }
    else {
      adurs <- adurs[(adurs$Minutes > range[1]) &
                       (adurs$Minutes <= range[2]), ]
      if (nrow(adurs) == 0) {
        a <- NA
        aa <- NA
      }
      else {
        a <- mean(adurs$Duration)
        aa <- median(adurs$Duration)
      }
    }
  }
  else {
    if (!is.data.frame(adurs)) {
      a <- NA
      aa <- NA
    } else {
      a <- mean(adurs$Duration)
      aa <- median(adurs$Duration)
    }
  }
  
  if (is.na(a) || is.nan(a))
    a <- NA
  if (is.na(aa) || is.nan(aa))
    aa <- NA
  tmp <- data.frame(a, aa)
  
  names(tmp) <- c("MeanDur", "MedianDur")
  tmp
}
Feeding.IntensitySummary.Well <- function(dfm, well, range = c(0, 0)) {
  d <- BaselineData.Well(dfm, well, range)
  l <- Expand.Events(FeedingData.Well.Events(dfm, well, range))
  
  da <- d[l]
  
  if (length(da) == 0) {
    a <- NA
    aa <- NA
    aaa <- NA
    aaaa <- NA
  }
  else {
    a <- mean(da)
    aa <- median(da)
    aaa <- min(da)
    aaaa <- max(da)
  }
  
  tmp <- data.frame(a, aa, aaa, aaaa)
  names(tmp) <- c("MeanInt", "MedianInt", "MinInt", "MaxInt")
  tmp
}
Feeding.Summary.OneWell <- function(dfm,
                                    range = c(0, 0),
                                    TransformLicks = TRUE) {
  if (dfm$Parameters$Chamber.Size != 1)
    stop("This function is for single chambers only")
  lights.sec <- (apply(GetLightsInfo(dfm, range)[, 2:13], 2, sum)) / dfm$Parameters$Samples.Per.Second
  for (i in 1:12) {
    interval <- Feeding.IntervalSummary.Well(dfm, i, range)
    intensity <- Feeding.IntensitySummary.Well(dfm, i, range)
    dur <- Feeding.DurationSummary.Well(dfm, i, range)
    FLicks <- Feeding.TotalLicks.Well(dfm, i, range)
    FEvents <- Feeding.TotalEvents.Well(dfm, i, range)
    if (i == 1)
      result <- data.frame(matrix(
        c(
          dfm$ID,
          i,
          FLicks,
          FEvents,
          unlist(dur),
          unlist(interval),
          unlist(intensity),
          lights.sec[i],
          range[1],
          range[2]
        ),
        nrow = 1
      ))
    else {
      tmp <- data.frame(matrix(
        c(
          dfm$ID,
          i,
          FLicks,
          FEvents,
          unlist(dur),
          unlist(interval),
          unlist(intensity),
          lights.sec[i],
          range[1],
          range[2]
        ),
        nrow = 1
      ))
      result <- rbind(result, tmp)
    }
  }
  
  names(result) <- c(
    "DFM",
    "Chamber",
    "Licks",
    "Events",
    "MeanDuration",
    "MedDuration",
    "MeanTimeBtw",
    "MedTimeBtw",
    "MeanInt",
    "MedianInt",
    "MinInt",
    "MaxInt",
    "OptoOn_sec",
    "StartMin",
    "EndMin"
  )
  if (TransformLicks == TRUE)
    result$Licks <- result$Licks^0.25
  result
  
}
Feeding.Summary.TwoWell <- function(dfm,
                                    range = c(0, 0),
                                    TransformLicks = TRUE) {
  if (dfm$Parameters$Chamber.Size != 2)
    stop("This function is for two-chamber DFM only")
  
  lights.sec <- (apply(GetLightsInfo(dfm, range)[, 2:13], 2, sum)) / dfm$Parameters$Samples.Per.Second
  
  for (i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
    if (dfm$Parameters$PI.Multiplier == 1) {
      wellA <- dfm$Parameters$Chamber.Sets[i, 1]
      wellB <- dfm$Parameters$Chamber.Sets[i, 2]
    }
    else {
      wellB <- dfm$Parameters$Chamber.Sets[i, 1]
      wellA <- dfm$Parameters$Chamber.Sets[i, 2]
    }
    
    interval.a <- Feeding.IntervalSummary.Well(dfm, wellA, range)
    intensity.a <- Feeding.IntensitySummary.Well(dfm, wellA, range)
    dur.a <- Feeding.DurationSummary.Well(dfm, wellA, range)
    FLicks.a <- Feeding.TotalLicks.Well(dfm, wellA, range)
    FEvents.a <- Feeding.TotalEvents.Well(dfm, wellA, range)
    
    interval.b <- Feeding.IntervalSummary.Well(dfm, wellB, range)
    intensity.b <- Feeding.IntensitySummary.Well(dfm, wellB, range)
    dur.b <- Feeding.DurationSummary.Well(dfm, wellB, range)
    FLicks.b <- Feeding.TotalLicks.Well(dfm, wellB, range)
    FEvents.b <- Feeding.TotalEvents.Well(dfm, wellB, range)
    
    lights.a <- lights.sec[wellA]
    lights.b <- lights.sec[wellB]
    
    FPIs <- c((FLicks.a - FLicks.b) / (FLicks.a + FLicks.b),
              (FEvents.a - FEvents.b) / (FEvents.a + FEvents.b)
    )
    
    
    
    if (i == 1) {
      result <- data.frame(matrix(
        c(
          dfm$ID,
          i,
          FPIs,
          FLicks.a,
          FLicks.b,
          FEvents.a,
          FEvents.b,
          unlist(dur.a),
          unlist(dur.b),
          unlist(interval.a),
          unlist(interval.b),
          unlist(intensity.a),
          unlist(intensity.b),
          lights.a,
          lights.b,
          range[1],
          range[2]
        ),
        nrow = 1
      ))
      
    }
    else {
      tmp <- data.frame(matrix(
        c(
          dfm$ID,
          i,
          FPIs,
          FLicks.a,
          FLicks.b,
          FEvents.a,
          FEvents.b,
          unlist(dur.a),
          unlist(dur.b),
          unlist(interval.a),
          unlist(interval.b),
          unlist(intensity.a),
          unlist(intensity.b),
          lights.a,
          lights.b,
          range[1],
          range[2]
        ),
        nrow = 1
      ))
      result <- rbind(result, tmp)
    }
  }
  names(result) <- c(
    "DFM",
    "Chamber",
    "PI",
    "EventPI",
    "LicksA",
    "LicksB",
    "EventsA",
    "EventsB",
    "MeanDurationA",
    "MedDurationA",
    "MeanDurationB",
    "MedDurationB",
    "MeanTimeBtwA",
    "MedTimeBtwA",
    "MeanTimeBtwB",
    "MedTimeBtwB",
    "MeanIntA",
    "MedianIntA",
    "MinIntA",
    "MaxIntA",
    "MeanIntB",
    "MedianIntB",
    "MinIntB",
    "MaxIntB",
    "OptoOn_sec_A",
    "OptoOn_sec_B",
    "StartMin",
    "EndMin"
  )
  if (TransformLicks == TRUE) {
    result$LicksA <- result$LicksA^0.25
    result$LicksB <- result$LicksB^0.25
  }
  result
  
}
AggregateTreatmentsBinnedData <- function(results) {
  trt.summary1 <- aggregate(
    results,
    by = list(results$Interval, results$Treatment),
    mean,
    na.rm = TRUE
  )
  trt.summary2 <- aggregate(results, by = list(results$Interval, results$Treatment), mySEM)
  trt.summary2 <- trt.summary2[, -grep("Treatment|DFM|Chamber|Interval", colnames(trt.summary2))]
  trt.summary1 <- trt.summary1[, -grep("Treatment|DFM|Chamber|Interval", colnames(trt.summary1))]
  
  ## This just indicates a two well chamber
  if ("LicksA" %in% names(results)) {
    tmp <- names(trt.summary1)[4:25]
    tmps <- paste(tmp, "SEM", sep = "")
    tmp <- names(trt.summary1)
    tmp <- c(tmp[1:25], tmps, tmp[26:ncol(trt.summary1)])
    tmp[1] <- "Interval"
    tmp[2] <- "Treatment"
    trt.summary <- data.frame(trt.summary1[, 1:25], trt.summary2[, 4:25], trt.summary1[, 26:ncol(trt.summary1)])
    names(trt.summary) <- tmp
  }
  else {
    trt.summary <- data.frame(trt.summary1[, 1:13], trt.summary2[, 4:13], trt.summary1[, 14:ncol(trt.summary1)])
    names(trt.summary1)[names(trt.summary1) == "Group.1"] <- "Interval"
    names(trt.summary1)[names(trt.summary1) == "Group.2"] <- "Treatment"
    tmp <- names(trt.summary1)[4:13]
    tmp <- paste(tmp, "SEM", sep = "")
    names(trt.summary) <- c(names(trt.summary1)[1:13], tmp, names(trt.summary1)[14:ncol(trt.summary1)])
  }
  trt.summary
}
AggregateTreatments <- function(results) {
  trt.summary1 <- aggregate(results,
                            by = list(results$Treatment),
                            mean,
                            na.rm = TRUE)
  trt.summary2 <- aggregate(results, by = list(results$Treatment), mySEM)
  trt.summary1 <- trt.summary1[, -grep("Treatment|DFM|Chamber", colnames(trt.summary1))]
  trt.summary2 <- trt.summary2[, -grep("Treatment|DFM|Chamber", colnames(trt.summary2))]
  
  if ("LicksA" %in% names(results)) {
    names(trt.summary1)[names(trt.summary1) == "Group.1"] <- "Treatment"
    tmp <- names(trt.summary1)[2:23]
    tmp <- paste(tmp, "SEM", sep = "")
    trt.summary <- data.frame(trt.summary1[, 1:23], trt.summary2[, 2:23], trt.summary1[, 24:ncol(trt.summary1)])
    names(trt.summary) <- c(names(trt.summary1)[1:23], tmp, names(trt.summary1)[24:ncol(trt.summary1)])
  }
  else {
    names(trt.summary1)[names(trt.summary1) == "Group.1"] <- "Treatment"
    tmp <- names(trt.summary1)[2:11]
    tmp <- paste(tmp, "SEM", sep = "")
    trt.summary <- data.frame(trt.summary1[, 1:11], trt.summary2[, 2:11], trt.summary1[, 12:ncol(trt.summary1)])
    names(trt.summary) <- c(names(trt.summary1)[1:11], tmp, names(trt.summary1)[12:ncol(trt.summary1)])
  }
  
  
  #tmp<-c("Treatment","MeanLicks","MeanEvents","MeanMDuration","MeanMedDuration","MeanMTimeBtw","MeanMedTimeBtw","MeanMInt","MeanMedInt",
  #       "SEMLicks","SEMEvents","SEMMDuration","SEMMedDuration","SEMMTimeBtw","SEMMedTimeBtw","SEMMInt","SEMMedInt",names(trt.summary)[18:ncol(trt.summary)])
  
  #names(trt.summary)<-tmp
  trt.summary
}


##### Data output helper functions. These can not be generalized. #####
OutputBaselinedData.Monitors <- function(monitors,
                                         parameters,
                                         range = c(0, 0),
                                         filename = "Baselined") {
  individ.params <- FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if (is.list(parameters[[1]]) == TRUE) {
    if (length(parameters) != length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params <- TRUE
  }
  
  for (j in 1:length(monitors)) {
    print(paste("Outputting Baselined Data for DFM ", monitors[j], ".", sep =
                  ""))
    flush.console()
    monitor <- monitors[j]
    if (individ.params == TRUE)
      p <- parameters[[j]]
    else
      p <- parameters
    dfm <- DFMClass(monitor, p)
    OutputBaselinedData.DFM(dfm, range, filename)
  }
}
OutputIntervalData.Monitors <- function(monitors,
                                        parameters,
                                        expDesign = NA,
                                        range = c(0, 0),
                                        filename = "IntervalData") {
  individ.params <- FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if (is.list(parameters[[1]]) == TRUE) {
    if (length(parameters) != length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params <- TRUE
  }
  for (j in 1:length(monitors)) {
    ##print(paste("Outputting Interval Data for DFM ",monitors[j],".",sep=""))
    ##flush.console()
    monitor <- monitors[j]
    if (individ.params == TRUE)
      p <- parameters[[j]]
    else
      p <- parameters
    dfm <- DFMClass(monitor, p)
    tmp2 <- GetIntervalData.DFM(dfm, range)
    if (is.data.frame(expDesign))
      tmp2 <- AppendTreatmentonResultsFrame(tmp2, expDesign)
    if (j == 1) {
      result <- tmp2
    }
    else {
      result <- rbind(result, tmp2)
    }
  }
  filename <- paste(filename, ".csv", sep = "")
  write.csv(result, file = filename, row.names = FALSE)
}
OutputDurationData.Monitors <- function(monitors,
                                        parameters,
                                        expDesign = NA,
                                        range = c(0, 0),
                                        filename = "DurationsData") {
  individ.params <- FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if (is.list(parameters[[1]]) == TRUE) {
    if (length(parameters) != length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params <- TRUE
  }
  for (j in 1:length(monitors)) {
    ##print(paste("Outputting Interval Data for DFM ",monitors[j],".",sep=""))
    ##flush.console()
    monitor <- monitors[j]
    if (individ.params == TRUE)
      p <- parameters[[j]]
    else
      p <- parameters
    dfm <- DFMClass(monitor, p)
    tmp2 <- GetDurationData.DFM(dfm, range)
    if (is.data.frame(expDesign))
      tmp2 <- AppendTreatmentonResultsFrame(tmp2, expDesign)
    if (j == 1) {
      result <- tmp2
    }
    else {
      result <- rbind(result, tmp2)
    }
  }
  filename <- paste(filename, ".csv", sep = "")
  write.csv(result, file = filename, row.names = FALSE)
}
## This fucntion will output for each well in each chamber for each monitor
## the total amount of time spend drinking over the perscribed range.
OutputTotalFeeding.Monitors <- function(monitors,
                                        parameters,
                                        expDesign = NA,
                                        range = c(0, 0),
                                        filename = "TotalFeedingTime") {
  individ.params <- FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if (is.list(parameters[[1]]) == TRUE) {
    if (length(parameters) != length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params <- TRUE
  }
  
  for (j in 1:length(monitors)) {
    monitor <- monitors[j]
    x <- 1:12
    if (individ.params == TRUE)
      p <- parameters[[j]]
    else
      p <- parameters
    dfm <- DFMClass(monitor, p)
    parameter.vector <- GetParameterVector(p)
    pnames <- Get.Parameter.Names(p)
    tmp <- Feeding.Summary.DFM(dfm, range)
    if (p$Chamber.Size == 1) {
      atotal <- tmp$Events * tmp$MeanDuration
      d <- tmp$DFM
      c <- tmp$Chamber
      tmp2 <- matrix(
        rep(parameter.vector, length(d)),
        ncol = length(parameter.vector),
        byrow = TRUE
      )
      tmp3 <- data.frame(d, c, atotal, tmp2)
      names(tmp3) <- c("DFM", "Chamber", "TotalSec", pnames)
      if (j == 1) {
        result <- tmp3
      }
      else {
        result <- rbind(result, tmp3)
      }
    }
    else if (p$Chamber.Size == 2) {
      atotal <- tmp$EventsA * tmp$MeanDurationA
      btotal <- tmp$EventsB * tmp$MeanDurationB
      d <- tmp$DFM
      c <- tmp$Chamber
      tmp2 <- matrix(
        rep(parameter.vector, length(d)),
        ncol = length(parameter.vector),
        byrow = TRUE
      )
      tmp3 <- data.frame(d, c, atotal, btotal, tmp2)
      names(tmp3) <- c("DFM", "Chamber", "ATotalSec", "BTotalSec", pnames)
      if (j == 1) {
        result <- tmp3
      }
      else {
        result <- rbind(result, tmp3)
      }
    }
    else
      stop("Feeding Summary not implemented for this DFM type.")
  }
  
  if (is.data.frame(expDesign))
    result <- AppendTreatmentonResultsFrame(result, expDesign)
  
  filename <- paste(filename, ".csv", sep = "")
  write.csv(result, file = filename, row.names = FALSE)
}


##### Plotting helper functions. #####
BinnedPlot.OneWell.Trt <- function(binnedDataResult,
                                   Type = "Licks",
                                   SaveToFile = FALSE) {
  tmp <- binnedDataResult$Stats
  
  pd <- position_dodge(5) # move them .05 to the left and right
  
  if (Type == "Licks") {
    ylabel <- "Licks"
    gp <- ggplot(tmp,
                 aes(
                   x = Minutes,
                   y = Licks,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = Licks - LicksSEM,
          ymax = Licks + LicksSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab(ylabel)
    filename <- paste("BinnedLicksPlots.pdf", sep = "")
    analysis <- data.frame(
      binnedDataResult$Results$Interval,
      binnedDataResult$Results$Treatment,
      binnedDataResult$Results$Licks
    )
    names(analysis) <- c("Interval", "Treatment", "Y")
  }
  else if (Type == "Events") {
    ylabel <- "Events"
    gp <- ggplot(tmp,
                 aes(
                   x = Minutes,
                   y = Events,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = Events - EventsSEM,
          ymax = Events + EventsSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab("Events")
    filename <- paste("BinnedEventsPlots.pdf", sep = "")
    analysis <- data.frame(
      binnedDataResult$Results$Interval,
      binnedDataResult$Results$Treatment,
      binnedDataResult$Results$Events
    )
    names(analysis) <- c("Interval", "Treatment", "Y")
  }
  else if (Type == "Durations") {
    gp <- ggplot(tmp,
                 aes(
                   x = Minutes,
                   y = MeanDuration,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = MeanDuration - MeanDurationSEM,
          ymax = MeanDuration + MeanDurationSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab("Mean Event Duration")
    filename <- paste("BinnedDurationPlots.pdf", sep = "")
    analysis <- data.frame(
      binnedDataResult$Results$Interval,
      binnedDataResult$Results$Treatment,
      binnedDataResult$Results$MeanDuration
    )
    names(analysis) <- c("Interval", "Treatment", "Y")
  }
  else if (Type == "MinInt") {
    gp <- ggplot(tmp,
                 aes(
                   x = Minutes,
                   y = MinInt,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = MinInt - MinIntSEM,
          ymax = MinInt + MinIntSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab("Min Event Intensity")
    filename <- paste("BinnedMinIntPlots.pdf", sep = "")
    analysis <- data.frame(
      binnedDataResult$Results$Interval,
      binnedDataResult$Results$Treatment,
      binnedDataResult$Results$MinInt
    )
    names(analysis) <- c("Interval", "Treatment", "Y")
  }
  else if (Type == "TimeBtw") {
    gp <- ggplot(tmp,
                 aes(
                   x = Minutes,
                   y = MeanTimeBtw,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = MeanTimeBtw - MeanTimeBtwSEM,
          ymax = MeanTimeBtw + MeanTimeBtwSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab("Time Between Events (sec)")
    filename <- paste("BinnedMeanTimeBtwPlots.pdf", sep = "")
    analysis <- data.frame(
      binnedDataResult$Results$Interval,
      binnedDataResult$Results$Treatment,
      binnedDataResult$Results$MeanTimeBtw
    )
    names(analysis) <- c("Interval", "Treatment", "Y")
  }
  else if (Type == "PI") {
    stop("Plot type not supported for single-well chambers.")
  }
  else if (Type == "EventPI") {
    stop("Plot type not supported for single-well chambers.")
  }
  else {
    stop("Plot type does not exist.")
  }
  show(gp)
  if (SaveToFile == TRUE) {
    ggsave(filename, gp)
  }
  
  l <- lapply(split(analysis, analysis$Interval), aov, formula = Y ~ Treatment)
  cat("** Interval specific ANOVA results **\n\n")
  lapply(l, summary)
  
}
BinnedPlot.TwoWell.Trt <- function(binnedDataResult,
                                   Type = "Licks",
                                   SaveToFile = FALSE) {
  stats <- binnedDataResult$Stats
  results <- binnedDataResult$Results
  
  tmp <- c(
    "LicksA",
    "EventsA",
    "MeanDurationA",
    "MedDurationA",
    "MeanTimeBtwA",
    "MedTimeBtwA",
    "MeanIntA",
    "MedianIntA",
    "MinIntA",
    "MaxIntA"
  )
  tmp2 <- paste(tmp, "SEM", sep = "")
  a.wells <- c("Treatment", "Minutes", tmp, tmp2)
  
  tmp <- c(
    "LicksB",
    "EventsB",
    "MeanDurationB",
    "MedDurationB",
    "MeanTimeBtwB",
    "MedTimeBtwB",
    "MeanIntB",
    "MedianIntB",
    "MinIntB",
    "MaxIntB"
  )
  tmp2 <- paste(tmp, "SEM", sep = "")
  b.wells <- c("Treatment", "Minutes", tmp, tmp2)
  
  tmp <- c(
    "Licks",
    "Events",
    "MeanDuration",
    "MedDuration",
    "MeanTimeBtw",
    "MedTimeBtw",
    "MeanInt",
    "MedianInt",
    "MinInt",
    "MaxInt"
  )
  tmp2 <- paste(tmp, "SEM", sep = "")
  new.names <- c("Treatment", "Minutes", tmp, tmp2, "Well")
  tmpA <- data.frame(stats[, a.wells], rep("WellA", nrow(stats)))
  tmpB <- data.frame(stats[, b.wells], rep("WellB", nrow(stats)))
  names(tmpA) <- new.names
  names(tmpB) <- new.names
  newData <- rbind(tmpA, tmpB)
  
  pd <- position_dodge(5) # move them .05 to the left and right
  
  if (Type == "Licks") {
    ylabel <- "Licks"
    gp <- ggplot(newData,
                 aes(
                   x = Minutes,
                   y = Licks,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = Licks - LicksSEM,
          ymax = Licks + LicksSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) + facet_wrap( ~ Well) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab(ylabel)
    filename <- paste("BinnedLicksPlots.pdf", sep = "")
    analysis <- data.frame(results$Interval,
                           results$Treatment,
                           results$LicksA,
                           results$LicksB)
    names(analysis) <- c("Interval", "Treatment", "YA", "YB")
  }
  else if (Type == "Events") {
    ylabel <- "Events"
    gp <- ggplot(newData,
                 aes(
                   x = Minutes,
                   y = Events,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = Events - EventsSEM,
          ymax = Events + EventsSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) + facet_wrap( ~ Well) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab("Events")
    filename <- paste("BinnedEventsPlots.pdf", sep = "")
    analysis <- data.frame(results$Interval,
                           results$Treatment,
                           results$EventsA,
                           results$EventsB)
    names(analysis) <- c("Interval", "Treatment", "YA", "YB")
  }
  else if (Type == "Durations") {
    gp <- ggplot(newData,
                 aes(
                   x = Minutes,
                   y = MeanDuration,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = MeanDuration - MeanDurationSEM,
          ymax = MeanDuration + MeanDurationSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) + facet_wrap( ~ Well) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab("Event Duration (sec)")
    filename <- paste("BinnedDurationPlots.pdf", sep = "")
    analysis <- data.frame(
      results$Interval,
      results$Treatment,
      results$MeanDurationA,
      results$MeanDurationB
    )
    names(analysis) <- c("Interval", "Treatment", "YA", "YB")
  }
  else if (Type == "MinInt") {
    gp <- ggplot(newData,
                 aes(
                   x = Minutes,
                   y = MinInt,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = MinInt - MinIntSEM,
          ymax = MinInt + MinIntSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) + facet_wrap( ~ Well) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab("Min Event Intensity")
    filename <- paste("BinnedMinIntPlots.pdf", sep = "")
    analysis <- data.frame(results$Interval,
                           results$Treatment,
                           results$MinIntA,
                           results$MinIntB)
    names(analysis) <- c("Interval", "Treatment", "YA", "YB")
  }
  else if (Type == "TimeBtw") {
    gp <- ggplot(newData,
                 aes(
                   x = Minutes,
                   y = MeanTimeBtw,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = MeanTimeBtw - MeanTimeBtwSEM,
          ymax = MeanTimeBtw + MeanTimeBtwSEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) + facet_wrap( ~ Well) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        fill = "white"
      ) + xlab("Minutes") + ylab("Time Between Events (sec)")
    filename <- paste("BinnedMeanTimeBtwPlots.pdf", sep = "")
    analysis <- data.frame(
      results$Interval,
      results$Treatment,
      results$MeanTimeBtwA,
      results$MeanTimeBtwB
    )
    names(analysis) <- c("Interval", "Treatment", "YA", "YB")
  }
  else if (Type == "PI") {
    Licks <- stats$LicksA + stats$LicksB
    stats <- data.frame(stats, Licks)
    gp <- ggplot(stats,
                 aes(
                   x = Minutes,
                   y = PI,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = PI - PISEM,
          ymax = PI + PISEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) + ylim(c(-1.05, 1.05)) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        aes(fill = Licks)
      ) + xlab("Minutes") + ylab("PI (Licks)")
    filename <- paste("BinnedPIPlots.pdf", sep = "")
    analysis <- data.frame(results$Interval,
                           results$Treatment,
                           results$PI,
                           results$PI)
    names(analysis) <- c("Interval", "Treatment", "YA", "YB")
  }
  else if (Type == "EventPI") {
    Events <- stats$EventsA + stats$EventsB
    stats <- data.frame(stats, Events)
    gp <- ggplot(stats,
                 aes(
                   x = Minutes,
                   y = EventPI,
                   color = Treatment,
                   group = Treatment
                 )) +
      geom_errorbar(
        aes(
          ymin = EventPI - EventPISEM,
          ymax = EventPI + EventPISEM,
          color = Treatment
        ),
        width = .1,
        position = pd
      ) +
      geom_line(position = pd, size = 1) + ylim(c(-1.05, 1.05)) +
      geom_point(
        position = pd,
        size = 4,
        shape = 21,
        aes(fill = Events)
      ) + xlab("Minutes") + ylab("Event PI")
    filename <- paste("BinnedEventPIPlots.pdf", sep = "")
    analysis <- data.frame(results$Interval,
                           results$Treatment,
                           results$EventPI,
                           results$EventPI)
    names(analysis) <- c("Interval", "Treatment", "YA", "YB")
  }
  else {
    stop("Plot type does not exist.")
  }
  show(gp)
  if (SaveToFile == TRUE) {
    ggsave(filename, gp)
  }
  l <- lapply(split(analysis, analysis$Interval), aov, formula = YA ~ Treatment)
  cat("\n\n\n** Interval specific ANOVA results for Well A **\n\n")
  print(lapply(l, summary))
  
  l <- lapply(split(analysis, analysis$Interval), aov, formula = YB ~ Treatment)
  cat("\n\n\n** Interval specific ANOVA results for Well B **\n\n")
  print(lapply(l, summary))
}
SimpleDataPlot.OneWell <- function(summaryResults,
                                   Type = "Licks",
                                   SaveToFile = FALSE) {
  results <- summaryResults$Results
  results <- subset(results, Treatment != "None")
  
  if (Type == "Licks") {
    filename <- paste("SimpleLicksPlot.pdf", sep = "")
    ylabel <- "Licks"
    r <- "Licks"
    gp <- (
      ggplot(results, aes(Treatment, Licks)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                              -1) + geom_jitter(size = 3, height = 0) +
        ylim(c(
          min(results$Licks), max(results$Licks)
        )) + ggtitle(r) + xlab("Treatment") + ylab(ylabel) + guides(fill = FALSE)
    )
    analysis <- data.frame(results$Treatment, results$Licks)
    names(analysis) <- c("Treatment", "Y")
  }
  else if (Type == "Events") {
    filename <- paste("SimpleEventsPlot.pdf", sep = "")
    ylabel <- "Events"
    r <- "Events"
    gp <- (
      ggplot(results, aes(Treatment, Events)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                               -1) + geom_jitter(size = 3, height = 0) +
        ylim(c(
          min(results$Events), max(results$Events)
        )) + ggtitle(r) + xlab("Treatment") + ylab(ylabel) + guides(fill = FALSE)
    )
    analysis <- data.frame(results$Treatment, results$Events)
    names(analysis) <- c("Treatment", "Y")
  }
  else if (Type == "Durations") {
    filename <- paste("SimpleDurationsPlot.pdf", sep = "")
    ylabel <- "Duration (sec)"
    r <- "Duration"
    gp <- (
      ggplot(results, aes(Treatment, MeanDuration)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                     -1) + geom_jitter(size = 3, height = 0) +
        ylim(c(
          min(results$MeanDuration), max(results$MeanDuration)
        )) + ggtitle(r) + xlab("Treatment") + ylab(ylabel) + guides(fill = FALSE)
    )
    analysis <- data.frame(results$Treatment, results$MeanDuration)
    names(analysis) <- c("Treatment", "Y")
    
  }
  else if (Type == "MinInt") {
    filename <- paste("SimpleMinIntPlot.pdf", sep = "")
    ylabel <- "Minimum Event Intensity"
    r <- "Min Intensity"
    gp <- (
      ggplot(results, aes(Treatment, MinInt)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                               -1) + geom_jitter(size = 3, height = 0) +
        ylim(c(
          min(results$MinInt), max(results$MinInt)
        )) + ggtitle(r) + xlab("Treatment") + ylab(ylabel) + guides(fill = FALSE)
    )
    analysis <- data.frame(results$Treatment, results$MinInt)
    names(analysis) <- c("Treatment", "Y")
    
  }
  else if (Type == "TimeBtw") {
    filename <- paste("SimpleMeantTimeBtwPlot.pdf", sep = "")
    ylabel <- "Time Between Events (sec)"
    r <- "Time Btw"
    gp <- (
      ggplot(results, aes(Treatment, MeanTimeBtw)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                    -1) + geom_jitter(size = 3, height = 0) +
        ylim(c(
          min(results$MeanTimeBtw), max(results$MeanTimeBtw)
        )) + ggtitle(r) + xlab("Treatment") + ylab(ylabel) + guides(fill = FALSE)
    )
    analysis <- data.frame(results$Treatment, results$MeanTimeBtw)
    names(analysis) <- c("Treatment", "Y")
  }
  else if (Type == "PI") {
    stop("Plot type not supported for single-well chambers.")
  }
  else if (Type == "EventPI") {
    stop("Plot type not supported for single-well chambers.")
  }
  else {
    stop("Plot type does not exist.")
  }
  show(gp)
  if (SaveToFile == TRUE)
    ggsave(filename = filename)
  r2 <- paste("\n** ", r, " **\n")
  cat(r2)
  print(summary(aov(Y ~ Treatment, data = analysis)))
}
SimpleDataPlot.TwoWell <- function(summaryResults,
                                   Type = "Licks",
                                   SaveToFile = FALSE) {
  results <- summaryResults$Results
  results <- subset(results, Treatment != "None")
  
  a.wells <- c(
    "Treatment",
    "LicksA",
    "EventsA",
    "MeanDurationA",
    "MedDurationA",
    "MeanTimeBtwA",
    "MedTimeBtwA",
    "MeanIntA",
    "MedianIntA",
    "MinIntA",
    "MaxIntA"
  )
  b.wells <- c(
    "Treatment",
    "LicksB",
    "EventsB",
    "MeanDurationB",
    "MedDurationB",
    "MeanTimeBtwB",
    "MedTimeBtwB",
    "MeanIntB",
    "MedianIntB",
    "MinIntB",
    "MaxIntB"
  )
  new.names <- c(
    "Treatment",
    "Licks",
    "Events",
    "MeanDuration",
    "MedDuration",
    "MeanTimeBtw",
    "MedTimeBtw",
    "MeanInt",
    "MedianInt",
    "MinInt",
    "MaxInt",
    "Well"
  )
  tmpA <- data.frame(results[, a.wells], rep("WellA", nrow(results)))
  tmpB <- data.frame(results[, b.wells], rep("WellB", nrow(results)))
  names(tmpA) <- new.names
  names(tmpB) <- new.names
  newData <- rbind(tmpA, tmpB)
  
  if (Type == "Licks") {
    filename <- paste("SimpleLicksPlot.pdf", sep = "")
    ylabel <- "Licks"
    r <- "Licks"
    gp <- ggplot(newData, aes(Treatment, Licks)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                  -1) + geom_jitter(size = 3, height = 0) + facet_wrap( ~ Well) +
      ylim(c(min(newData$Licks), max(newData$Licks))) + ggtitle(r) + xlab("Treatment") +
      ylab(ylabel) + guides(fill = FALSE)
    analysis <- data.frame(results$Treatment, results$LicksA, results$LicksB)
    names(analysis) <- c("Treatment", "YA", "YB")
  }
  else if (Type == "Events") {
    filename <- paste("SimpleEventsPlot.pdf", sep = "")
    ylabel <- "Events"
    r <- "Events"
    gp <- ggplot(newData, aes(Treatment, Events)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                   -1) + geom_jitter(size = 3, height = 0) + facet_wrap( ~ Well) +
      ylim(c(min(newData$Events), max(newData$Events))) + ggtitle(r) + xlab("Treatment") +
      ylab(ylabel) + guides(fill = FALSE)
    analysis <- data.frame(results$Treatment, results$EventsA, results$EventsB)
    names(analysis) <- c("Treatment", "YA", "YB")
  }
  else if (Type == "Durations") {
    filename <- paste("SimpleDurationsPlot.pdf", sep = "")
    ylabel <- "Duration (sec))"
    r <- "Duration"
    gp <- ggplot(newData, aes(Treatment, MeanDuration)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                         -1) + geom_jitter(size = 3, height = 0) + facet_wrap( ~ Well) +
      ylim(c(min(newData$MeanDuration), max(newData$MeanDuration))) + ggtitle(r) + xlab("Treatment") +
      ylab(ylabel) + guides(fill = FALSE)
    analysis <- data.frame(results$Treatment,
                           results$MeanDurationA,
                           results$MeanDurationB)
    names(analysis) <- c("Treatment", "YA", "YB")
    
  }
  else if (Type == "MinInt") {
    filename <- paste("SimpleMinIntPlot.pdf", sep = "")
    ylabel <- "Minimum Event Intensity"
    r <- "Min Intensity"
    gp <- ggplot(newData, aes(Treatment, MinInt)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                   -1) + geom_jitter(size = 3, height = 0) + facet_wrap( ~ Well) +
      ylim(c(min(newData$MinInt), max(newData$MinInt))) + ggtitle(r) + xlab("Treatment") +
      ylab(ylabel) + guides(fill = FALSE)
    analysis <- data.frame(results$Treatment, results$MinIntA, results$MinIntB)
    names(analysis) <- c("Treatment", "YA", "YB")
  }
  else if (Type == "TimeBtw") {
    filename <- paste("SimpleMeantTimeBtwPlot.pdf", sep = "")
    ylabel <- "Time Between Events (sec) (wmean(A,B))"
    r <- "Time Btw"
    gp <- ggplot(newData, aes(Treatment, MeanTimeBtw)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                        -1) + geom_jitter(size = 3, height = 0) + facet_wrap( ~ Well) +
      ylim(c(min(newData$MeanTimeBtw), max(newData$MeanTimeBtw))) + ggtitle(r) + xlab("Treatment") +
      ylab(ylabel) + guides(fill = FALSE)
    analysis <- data.frame(results$Treatment,
                           results$MeanTimeBtwA,
                           results$MeanTimeBtwB)
    names(analysis) <- c("Treatment", "YA", "YB")
  }
  else if (Type == "PI") {
    filename <- paste("SimplePIPlot.pdf", sep = "")
    ylabel <- "Licks PI"
    
    Licks <- results$LicksA + results$LicksB
    results <- data.frame(results, Licks)
    results <- results[Licks > 0, ]
    
    r <- "PI (Licks)"
    gp <- ggplot(results, aes(results$Treatment, results$PI)) + geom_boxplot(aes(fill = results$Treatment), outlier.size =
                                                                               -1) + geom_jitter(aes(color = Licks), size = 3, height = 0) +
      ylim(c(-1.05, 1.05)) + ggtitle(r) + xlab("Treatment") + ylab("PI") + guides(fill =
                                                                                    FALSE)
    
    analysis <- data.frame(results$Treatment, results$PI, results$PI)
    names(analysis) <- c("Treatment", "YA", "YB")
  }
  else if (Type == "EventPI") {
    filename <- paste("SimpleEventPIPlot.pdf", sep = "")
    ylabel <- "Event PI"
    
    Events <- results$EventsA + results$EventsB
    results <- data.frame(results, Events)
    results <- results[Events > 0, ]
    
    r <- "PI (Events)"
    gp <- ggplot(results, aes(results$Treatment, results$EventPI)) + geom_boxplot(aes(fill = results$Treatment), outlier.size =
                                                                                    -1) + geom_jitter(aes(color = Events), size = 3, height = 0) +
      ylim(c(-1.05, 1.05)) + ggtitle(r) + xlab("Treatment") + ylab("Event PI") + guides(fill =
                                                                                          FALSE)
    
    analysis <- data.frame(results$Treatment, results$EventPI, results$EventPI)
    names(analysis) <- c("Treatment", "YA", "YB")
  }
  else {
    stop("Plot type does not exist.")
  }
  show(gp)
  if (SaveToFile == TRUE)
    ggsave(filename = filename)
  
  r2 <- paste("\n** ", r, " **\n")
  cat(r2)
  cat("\nANOVA results for Well A\n")
  print(summary(aov(YA ~ Treatment, data = analysis)))
  
  cat("\n\nANOVA results for Well B\n")
  print(summary(aov(YB ~ Treatment, data = analysis)))
}

## Functions used for cumulative division plots of basic stats.
DivisionPlots.OneWell <- function(monitors,
                                  parameters,
                                  expDesign,
                                  range = c(0, 0),
                                  divisions = 1,
                                  Type = "Licks",
                                  SaveToFile = FALSE,
                                  TransformLicks = TRUE) {
  ranges <- matrix(rep(NA, divisions * 2), ncol = 2)
  if (divisions == 1)
    ranges[1, ] <- range
  else {
    if (range[2] == 0) {
      ## Come back to here
      dfm <- GetDFM(monitors[1])
      if (sum(is.na(dfm)))
        stop("DFM Missing")
      last.time <- LastSampleData(dfm)$Minutes
      breaks <- seq(from = range[1],
                    to = last.time,
                    length = divisions + 1)
      ranges.1 <- range[1]
      ranges.2 <- breaks[-1]
      ranges <- round(cbind(ranges.1, ranges.2))
    }
    else {
      breaks <- seq(from = range[1],
                    to = range[2],
                    length = divisions + 1)
      ranges.1 <- range[1]
      ranges.2 <- breaks[-1]
      ranges <- round(cbind(ranges.1, ranges.2))
    }
  }
  if (SaveToFile == TRUE) {
    filename <- paste("DivisionPlots_", Type, ".pdf", sep = "")
    pdf(file = filename)
  }
  if (divisions == 1) {
    tmp <- Feeding.Summary.Monitors(monitors,
                                    parameters,
                                    expDesign,
                                    range,
                                    FALSE,
                                    TransformLicks)
    results <- tmp$Results
    results <- subset(results, Treatment != "None")
    
    if (Type == "Licks") {
      if (TransformLicks == TRUE) {
        r <- paste("Transformed Licks -- Range(min): (",
                   range[1],
                   ",",
                   range[2],
                   ")",
                   sep = "")
        ylabel <- "Transformed Licks"
      }
      else {
        r <- paste("Licks -- Range(min): (", range[1], ",", range[2], ")", sep =
                     "")
        ylabel <- "Licks"
      }
      
      print(
        ggplot(results, aes(Treatment, Licks)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$Licks), max(results$Licks)
          )) + ggtitle(r) + xlab("Treatment") + ylab(ylabel) + guides(fill = FALSE)
      )
      r2 <- paste("\n** ", r, " **\n")
      cat(r2)
      print(summary(aov(Licks ~ Treatment, data = results)))
    }
    else if (Type == "Events") {
      r <- paste("Events-Range: (", range[1], ",", range[2], ")", sep = "")
      print(
        ggplot(results, aes(Treatment, Events)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                 -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$Events), max(results$Events)
          )) + ggtitle(r) + xlab("Treatment") + ylab("Events") + guides(fill = FALSE)
      )
      cat("** ANOVA results Full Range **\n\n")
      print(summary(aov(Events ~ Treatment, data = results)))
    }
    else if (Type == "Durations") {
      r <- paste("Durations -- Range(min): (", range[1], ",", range[2], ")", sep =
                   "")
      print(
        ggplot(results, aes(Treatment, MeanDuration)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                       -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$MeanDuration), max(results$MeanDuration)
          )) + ggtitle(r) + xlab("Treatment") + ylab("Mean Duration (sec)") + guides(fill =
                                                                                       FALSE)
      )
      print(summary(aov(MeanDuration ~ Treatment, data = results)))
    }
    else if (Type == "TimeBtw") {
      r <- paste("Time Btw -- Range(min): (", range[1], ",", range[2], ")", sep =
                   "")
      print(
        ggplot(results, aes(Treatment, MeanTimeBtw)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                      -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$MeanTimeBtw), max(results$MeanTimeBtw)
          )) + ggtitle(r) + xlab("Treatment") + ylab("Mean Time Between Events (sec)") + guides(fill =
                                                                                                  FALSE)
      )
      print(summary(aov(MeanTimeBtw ~ Treatment, data = results)))
    }
    else if (Type == "MinInt") {
      r <- paste("Min Intensity -- Range(min): (",
                 range[1],
                 ",",
                 range[2],
                 ")",
                 sep = "")
      print(
        ggplot(results, aes(Treatment, MinInt)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                 -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$MinInt), max(results$MinInt)
          )) + ggtitle(r) + xlab("Treatment") + ylab("Min Event Intensity") + guides(fill =
                                                                                       FALSE)
      )
      print(summary(aov(MinInt ~ Treatment, data = results)))
    }
    else if (Type == "PI") {
      stop("Plot type not supported for single-well chambers.")
    }
    else if (Type == "EventPI") {
      stop("Plot type not supported for single-well chambers.")
    }
    else {
      if (SaveToFile == TRUE) {
        graphics.off()
      }
      stop("Plot type does not exist.")
    }
  }
  else {
    p <- list()
    for (i in 1:divisions)
      local({
        tmp <- Feeding.Summary.Monitors(monitors,
                                        parameters,
                                        expDesign,
                                        ranges[i, ],
                                        FALSE,
                                        TransformLicks)
        results <- tmp$Results
        results <- subset(results, Treatment != "None")
        if (Type == "Licks") {
          if (TransformLicks == TRUE) {
            r <- paste("Transformed Licks -- Range(min): (",
                       ranges[i, 1],
                       ",",
                       ranges[i, 2],
                       ")",
                       sep = "")
            ylabel <- "Transformed Licks"
          }
          else {
            r <- paste("Licks -- Range(min): (",
                       ranges[i, 1],
                       ",",
                       ranges[i, 2],
                       ")",
                       sep = "")
            ylabel <- "Licks"
          }
          r2 <- paste("\n** ", r, " **\n")
          cat(r2)
          print(summary(aov(Licks ~ Treatment, data = results)))
          p[[i]] <<- (
            ggplot(results, aes(Treatment, Licks)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                    -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$Licks), max(results$Licks)
              )) + ggtitle(r) + xlab("Treatment") + ylab(ylabel) + guides(fill = FALSE)
          )
        }
        else if (Type == "Events") {
          r <- paste("Events-Range: (", ranges[i, 1], ",", ranges[i, 2], ")", sep =
                       "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, Events)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                     -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$Events), max(results$Events)
              )) + ggtitle(r) + xlab("Treatment") + ylab("Events") + guides(fill = FALSE)
          )
          ppp <- paste("\n** ANOVA results -- Range(min): (",
                       ranges[i, 1],
                       ",",
                       ranges[i, 2],
                       ")",
                       sep = "")
          cat(ppp)
          print(summary(aov(Events ~ Treatment, data = results)))
        }
        else if (Type == "Durations") {
          r <- paste("Durations -- Range(min): (",
                     ranges[i, 1],
                     ",",
                     ranges[i, 2],
                     ")",
                     sep = "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, MeanDuration)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                           -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$MeanDuration),
                max(results$MeanDuration)
              )) + ggtitle(r) + xlab("Treatment") + ylab("Mean Duration (sec)") + guides(fill =
                                                                                           FALSE)
          )
          print(summary(aov(MeanDuration ~ Treatment, data = results)))
        }
        else if (Type == "TimeBtw") {
          r <- paste("Time Btw -- Range(min): (",
                     ranges[i, 1],
                     ",",
                     ranges[i, 2],
                     ")",
                     sep = "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, MeanTimeBtw)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                          -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$MeanTimeBtw), max(results$MeanTimeBtw)
              )) + ggtitle(r) + xlab("Treatment") + ylab("Mean Time Between Events (sec)") + guides(fill =
                                                                                                      FALSE)
          )
          print(summary(aov(MeanTimeBtw ~ Treatment, data = results)))
        }
        else if (Type == "MinInt") {
          r <- paste("Min Intensity -- Range(min): (",
                     ranges[i, 1],
                     ",",
                     ranges[i, 2],
                     ")",
                     sep = "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, MinInt)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                     -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$MinInt), max(results$MinInt)
              )) + ggtitle(r) + xlab("Treatment") + ylab("Min Event Intensity") + guides(fill =
                                                                                           FALSE)
          )
          print(summary(aov(MinInt ~ Treatment, data = results)))
        }
        else if (Type == "PI") {
          stop("Plot type not supported for single-well chambers.")
        }
        else if (Type == "EventPI") {
          stop("Plot type not supported for single-well chambers.")
        }
        else {
          if (SaveToFile == TRUE) {
            graphics.off()
          }
          stop("Plot type does not exist.")
        }
      })
    if (divisions < 5)
      numcols <- 2
    else if (divisions < 10)
      numcols <- 3
    else if (divisions < 17)
      numcols <- 4
    else
      numcols <- 5
    multiplot(plotlist = p, cols = numcols)
  }
  if (SaveToFile == TRUE)
    graphics.off()
}
DivisionPlots.TwoWell <- function(monitors,
                                  parameters,
                                  expDesign,
                                  range = c(0, 0),
                                  divisions = 1,
                                  Type = "Licks",
                                  SaveToFile = FALSE,
                                  TransformLicks = TRUE) {
  ranges <- matrix(rep(NA, divisions * 2), ncol = 2)
  if (divisions == 1)
    ranges[1, ] <- range
  else {
    if (range[2] == 0) {
      dfm <- GetDFM(monitors[1])
      if (sum(is.na(dfm)))
        stop("DFM Missing")
      last.time <- LastSampleData(dfm)$Minutes
      breaks <- seq(from = range[1],
                    to = last.time,
                    length = divisions + 1)
      ranges.1 <- range[1]
      ranges.2 <- breaks[-1]
      ranges <- round(cbind(ranges.1, ranges.2))
    }
    else {
      breaks <- seq(from = range[1],
                    to = range[2],
                    length = divisions + 1)
      ranges.1 <- range[1]
      ranges.2 <- breaks[-1]
      ranges <- round(cbind(ranges.1, ranges.2))
    }
  }
  
  if (SaveToFile == TRUE) {
    filename <- paste("DivisionPlots_", Type, ".pdf", sep = "")
    pdf(file = filename)
  }
  
  if (divisions == 1) {
    tmp <- Feeding.Summary.Monitors(monitors,
                                    parameters,
                                    expDesign,
                                    range,
                                    FALSE,
                                    TransformLicks)
    results <- tmp$Results
    results <- subset(results, Treatment != "None")
    
    if (Type == "Licks") {
      Licks <- results$LicksA + results$LicksB
      if (TransformLicks == TRUE) {
        r <- paste("Transformed Total Licks -- Range(min): (",
                   range[1],
                   ",",
                   range[2],
                   ")",
                   sep = "")
        ylabel <- "Transformed Total Licks (A+B)"
      }
      else {
        r <- paste("Licks -- Range(min): (", range[1], ",", range[2], ")", sep =
                     "")
        ylabel <- "Total Licks (A+B)"
      }
      results <- data.frame(results, Licks)
      r2 <- paste("\n** ", r, " **\n")
      cat(r2)
      print(summary(aov(Licks ~ Treatment, data = results)))
      print(
        ggplot(results, aes(Treatment, Licks)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$Licks), max(results$Licks)
          )) + ggtitle(r) + xlab("Treatment") + ylab(ylabel) + guides(fill = FALSE)
      )
    }
    else if (Type == "Events") {
      Events <- results$EventsA + results$EventsB
      results <- data.frame(results, Events)
      r <- paste("Events-Range: (", range[1], ",", range[2], ")", sep =
                   "")
      print(
        ggplot(results, aes(Treatment, Events)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                 -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$Events), max(results$Events)
          )) + ggtitle(r) + xlab("Treatment") + ylab("Total Events (A+B)") + guides(fill =
                                                                                      FALSE)
      )
      cat("** ANOVA results Full Range **\n\n")
      print(summary(aov(Events ~ Treatment, data = results)))
    }
    else if (Type == "Durations") {
      MeanDuration <- ((results$MeanDurationA * results$EventsA) + (results$MeanDurationB *
                                                                      results$EventsB)
      ) / (results$EventsA + results$EventsB)
      results <- data.frame(results, MeanDuration)
      r <- paste("Durations -- Range(min): (", range[1], ",", range[2], ")", sep =
                   "")
      print(
        ggplot(results, aes(Treatment, MeanDuration)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                       -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$MeanDuration), max(results$MeanDuration)
          )) + ggtitle(r) + xlab("Treatment") + ylab("Mean Duration (A and B)") + guides(fill =
                                                                                           FALSE)
      )
      print(summary(aov(MeanDuration ~ Treatment, data = results)))
    }
    else if (Type == "TimeBtw") {
      MeanTimeBtw <- ((results$MeanTimeBtwA * results$EventsA) + (results$MeanTimeBtwB *
                                                                    results$EventsB)
      ) / (results$EventsA + results$EventsB)
      results <- data.frame(results, MeanTimeBtw)
      r <- paste("Time Btw -- Range(min): (", range[1], ",", range[2], ")", sep =
                   "")
      print(
        ggplot(results, aes(Treatment, MeanTimeBtw)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                      -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$MeanTimeBtw), max(results$MeanTimeBtw)
          )) + ggtitle(r) + xlab("Treatment") + ylab("Time Between") + guides(fill =
                                                                                FALSE)
      )
      print(summary(aov(MeanTimeBtw ~ Treatment, data = results)))
    }
    else if (Type == "MinInt") {
      MinInt <- pmin(results$MinIntA, results$MinIntB)
      results <- data.frame(results, MinInt)
      r <- paste("Min Intensity -- Range(min): (",
                 range[1],
                 ",",
                 range[2],
                 ")",
                 sep = "")
      print(
        ggplot(results, aes(Treatment, MinInt)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                 -1) + geom_jitter(size = 3, height = 0) +
          ylim(c(
            min(results$MinInt), max(results$MinInt)
          )) + ggtitle(r) + xlab("Treatment") + ylab("Min Event Intensity") + guides(fill =
                                                                                       FALSE)
      )
      print(summary(aov(MinInt ~ Treatment, data = results)))
    }
    else if (Type == "PI") {
      Licks <- results$LicksA + results$LicksB
      results <- data.frame(results, Licks)
      results <- results[Licks > 0, ]
      r <- paste("PI -- Range: (", range[1], ",", range[2], ")", sep = "")
      print(
        ggplot(results, aes(Treatment, PI)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                             -1) + geom_jitter(aes(color = Licks), size = 3, height = 0) +
          ylim(c(-1.05, 1.05)) + ggtitle(r) + xlab("Treatment") + ylab("PI") + guides(fill =
                                                                                        FALSE)
      )
      print(summary(aov(PI ~ Treatment, data = results)))
    }
    else if (Type == "EventPI") {
      Events <- results$EventsA + results$EventsB
      results <- data.frame(results, Events)
      results <- results[Events > 0, ]
      r <- paste("Event PI -- Range: (", range[1], ",", range[2], ")", sep =
                   "")
      print(
        ggplot(results, aes(Treatment, EventPI)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                  -1) + geom_jitter(aes(color = Events), size = 3, height = 0) +
          ylim(c(-1.05, 1.05)) + ggtitle(r) + xlab("Treatment") + ylab("Event PI") + guides(fill =
                                                                                              FALSE)
      )
      print(summary(aov(EventPI ~ Treatment, data = results)))
    }
    else {
      if (SaveToFile == TRUE) {
        graphics.off()
      }
      stop("Plot type does not exist.")
    }
  }
  else {
    p <- list()
    for (i in 1:divisions)
      local({
        ## Because we are adding licks together, we should transform only after adding!!
        ## So this function should always work on non-transformed data.
        tmp <- Feeding.Summary.Monitors(monitors,
                                        parameters,
                                        expDesign,
                                        ranges[i, ],
                                        FALSE,
                                        TransformLicks = FALSE)
        results <- tmp$Results
        results <- subset(results, Treatment != "None")
        if (Type == "Licks") {
          Licks <- results$LicksA + results$LicksB
          if (TransformLicks == TRUE) {
            r <- paste("Transformed Total Licks -- Range(min): (",
                       ranges[i, 1],
                       ",",
                       ranges[i, 2],
                       ")"
                       ,
                       sep = "")
            ylabel <- "Transformed Totoal Licks (A+B)"
            Licks <- Licks^0.25
          }
          else {
            r <- paste("Total Licks -- Range(min): (",
                       ranges[i, 1],
                       ",",
                       ranges[i, 2],
                       ")",
                       sep = "")
            ylabel <- "Total Licks (A+B)"
          }
          results <- data.frame(results, Licks)
          r2 <- paste("\n** ", r, " **\n")
          cat(r2)
          print(summary(aov(Licks ~ Treatment, data = results)))
          p[[i]] <<- (
            ggplot(results, aes(Treatment, Licks)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                    -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$Licks), max(results$Licks)
              )) + ggtitle(r) + xlab("Treatment") + ylab(ylabel) + guides(fill = FALSE)
          )
        }
        else if (Type == "Events") {
          Events <- results$EventsA + results$EventsB
          results <- data.frame(results, Events)
          r <- paste("Events-Range: (", ranges[i, 1], ",", ranges[i, 2], ")", sep =
                       "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, Events)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                     -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$Events), max(results$Events)
              )) + ggtitle(r) + xlab("Treatment") + ylab("Total Events (A+B)") + guides(fill =
                                                                                          FALSE)
          )
          ppp <- paste("\n** ANOVA results -- Range(min): (",
                       ranges[i, 1],
                       ",",
                       ranges[i, 2],
                       ")",
                       sep = "")
          cat(ppp)
          print(summary(aov(Events ~ Treatment, data = results)))
        }
        else if (Type == "Durations") {
          MeanDuration <- ((results$MeanDurationA * results$EventsA) + (results$MeanDurationB *
                                                                          results$EventsB)
          ) / (results$EventsA + results$EventsB)
          results <- data.frame(results, MeanDuration)
          r <- paste("Durations -- Range(min): (",
                     ranges[i, 1],
                     ",",
                     ranges[i, 2],
                     ")",
                     sep = "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, MeanDuration)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                           -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$MeanDuration),
                max(results$MeanDuration)
              )) + ggtitle(r) + xlab("Treatment") + ylab("Mean Duration (A and B") + guides(fill =
                                                                                              FALSE)
          )
          print(summary(aov(MeanDuration ~ Treatment, data = results)))
        }
        else if (Type == "TimeBtw") {
          MeanTimeBtw <- ((results$MeanTimeBtwA * results$EventsA) + (results$MeanTimeBtwB *
                                                                        results$EventsB)
          ) / (results$EventsA + results$EventsB)
          results <- data.frame(results, MeanTimeBtw)
          r <- paste("Time Btw -- Range(min): (",
                     ranges[i, 1],
                     ",",
                     ranges[i, 2],
                     ")",
                     sep = "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, MeanTimeBtw)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                          -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$MeanTimeBtw), max(results$MeanTimeBtw)
              )) + ggtitle(r) + xlab("Treatment") + ylab("Time Between") + guides(fill =
                                                                                    FALSE)
          )
          print(summary(aov(MeanTimeBtw ~ Treatment, data = results)))
        }
        else if (Type == "MinInt") {
          MinInt <- pmin(results$MinIntA, results$MinIntB)
          results <- data.frame(results, MinInt)
          r <- paste("Min Intensity -- Range(min): (",
                     ranges[i, 1],
                     ",",
                     ranges[i, 2],
                     ")",
                     sep = "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, MinInt)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                     -1) + geom_jitter(size = 3, height = 0) +
              ylim(c(
                min(results$MinInt), max(results$MinInt)
              )) + ggtitle(r) + xlab("Treatment") + ylab("Min Event Intensity") + guides(fill =
                                                                                           FALSE)
          )
          print(summary(aov(MinInt ~ Treatment, data = results)))
        }
        else if (Type == "PI") {
          Licks <- results$LicksA + results$LicksB
          results <- data.frame(results, Licks)
          results <- results[Licks > 0, ]
          r <- paste("PI -- Range: (", ranges[i, 1], ",", ranges[i, 2], ")", sep =
                       "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, PI)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                 -1) + geom_jitter(aes(color = Licks), size = 3, height = 0) +
              ylim(c(-1.05, 1.05)) + ggtitle(r) + xlab("Treatment") +
              ylab("PI") + guides(fill = FALSE)
          )
          print(summary(aov(PI ~ Treatment, data = results)))
        }
        else if (Type == "EventPI") {
          Events <- results$EventsA + results$EventsB
          results <- data.frame(results, Events)
          results <- results[Events > 0, ]
          r <- paste("Event PI -- Range: (", ranges[i, 1], ",", ranges[i, 2], ")", sep =
                       "")
          p[[i]] <<- (
            ggplot(results, aes(Treatment, EventPI)) + geom_boxplot(aes(fill = Treatment), outlier.size =
                                                                      -1) + geom_jitter(
                                                                        aes(color = Events),
                                                                        size = 3,
                                                                        height = 0
                                                                      ) +
              ylim(c(-1.05, 1.05)) + ggtitle(r) + xlab("Treatment") +
              ylab("Event PI") + guides(fill = FALSE)
          )
          print(summary(aov(EventPI ~ Treatment, data = results)))
        }
        else {
          if (SaveToFile == TRUE) {
            graphics.off()
          }
          stop("Plot type does not exist.")
        }
      })
    if (divisions < 5)
      numcols <- 2
    else if (divisions < 10)
      numcols <- 3
    else if (divisions < 17)
      numcols <- 4
    else
      numcols <- 5
    multiplot(plotlist = p, cols = numcols)
  }
  if (SaveToFile == TRUE)
    graphics.off()
}

## For the individual DFM plots.
PlotBins.Licks.DFM.OneWell <- function(dfm,
                                       binsize.min = 30,
                                       range = c(0, 0),
                                       TransformLicks = TRUE) {
  if (dfm$Parameters$Chamber.Size != 1)
    stop("This function is for single chambers only")
  binnedData <- BinnedFeeding.Summary.DFM(dfm, binsize.min, range, TransformLicks)
  ylabel <- "Licks"
  xlabel <- "Minutes"
  ttl <- paste("DFM:", dfm$ID)
  if (TransformLicks == TRUE) {
    ylabel <- "Transformed Licks"
    ttl <- paste("DFM:", dfm$ID, " (Transformed)")
  }
  gp <- ggplot(binnedData, aes(x = Minutes, y = Licks, fill = Chamber)) + geom_bar(stat =
                                                                                     "identity") + facet_grid(Chamber ~ .) + ggtitle(paste("DFM:", dfm$ID)) +
    theme(legend.position = "none") + ylab(ylabel) + xlab(xlabel)
  show(gp)
}
PlotBins.Licks.DFM.TwoWell <- function(dfm,
                                       binsize.min,
                                       range = c(0, 0),
                                       TransformLicks = TRUE) {
  if (dfm$Parameters$Chamber.Size != 2)
    stop("This function is for two-well chambers only")
  binnedData <- BinnedFeeding.Summary.DFM(dfm, binsize.min, range, TransformLicks)
  ylabel <- "Licks"
  xlabel <- "Minutes"
  ttl <- paste("DFM:", dfm$ID)
  if (TransformLicks == TRUE) {
    ylabel <- "Transformed Licks"
    ttl <- paste("DFM:", dfm$ID, " (Transformed)")
  }
  tmp2 <- melt(
    binnedData,
    id.vars = c("Minutes", "Chamber"),
    measure.vars = c("LicksA", "LicksB")
  )
  names(tmp2)[3] <- "Well"
  gp <- ggplot(tmp2, aes(x = Minutes, y = value, fill = Well)) + geom_bar(stat =
                                                                            "identity") + facet_grid(Chamber ~ .) + ggtitle(paste("DFM:", dfm$ID)) +
    ylab(ylabel) + xlab(xlabel)
  show(gp)
}
PlotBins.Durations.DFM.OneWell <- function(dfm,
                                           binsize.min = 30,
                                           range = c(0, 0)) {
  if (dfm$Parameters$Chamber.Size != 1)
    stop("This function is for single chambers only")
  binnedData <- BinnedFeeding.Summary.DFM(dfm, binsize.min, range, FALSE)
  ylabel <- "Avg Duration"
  xlabel <- "Minutes"
  ttl <- paste("DFM:", dfm$ID)
  gp <- ggplot(binnedData, aes(x = Minutes, y = Duration, fill = Chamber)) + geom_bar(stat =
                                                                                        "identity") + facet_grid(Chamber ~ .) + ggtitle(paste("DFM:", dfm$ID)) +
    theme(legend.position = "none") + ylab(ylabel) + xlab(xlabel)
  show(gp)
}
PlotBins.Durations.DFM.TwoWell <- function(dfm, binsize.min, range = c(0, 0)) {
  if (dfm$Parameters$Chamber.Size != 2)
    stop("This function is for two-well chambers only")
  binnedData <- BinnedFeeding.Summary.DFM(dfm, binsize.min, range, FALSE)
  ylabel <- "Avg Duration"
  xlabel <- "Minutes"
  ttl <- paste("DFM:", dfm$ID)
  tmp2 <- melt(
    binnedData,
    id.vars = c("Minutes", "Chamber"),
    measure.vars = c("DurationA", "DurationB")
  )
  names(tmp2)[3] <- "Well"
  gp <- ggplot(tmp2, aes(x = Minutes, y = value, fill = Well)) + geom_bar(stat =
                                                                            "identity") + facet_grid(Chamber ~ .) + ggtitle(paste("DFM:", dfm$ID)) +
    ylab(ylabel) + xlab(xlabel)
  show(gp)
}


##### Utility functions #####

## This function takes 2 vectors, one with the events
## above a minimal threshold (minvec) and one that
## specifies events that pass a more stringent threshold (maxvec).
## Contiguous events are only kept if at least one
## value in the event, which is defined by minvec, is above
## the higher threshold, which is defined by max vec
## z <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
## zz <- c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)
## Get.Surviving.Events(z,zz) -> (2 0 0 0 0 0 3 0 0)
Get.Surviving.Events <- function(minvec, maxvec) {
  tmp <- Get.Events(minvec)
  result <- tmp
  indices <- (1:length(minvec))[tmp > 0]
  for (i in indices) {
    tmp2 <- maxvec[i:(i + (tmp[i] - 1))]
    if (sum(tmp2) == 0)
      result[i] <- 0
  }
  result
}

## This function is the reverse of Get.Events
## (2 0 0 0 1 0 3 0 0) -> c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
Expand.Events <- function(eventvec) {
  result <- rep(FALSE, length(eventvec))
  indices <- (1:length(eventvec))[eventvec > 0]
  for (i in indices) {
    result[i:(i + eventvec[i] - 1)] <- TRUE
  }
  result
}

## These functions are helper functions for the basic calculations
# This function replaces continuing events with zero and make the first event of that
# episode equal to its duration.
## c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE) -> (2 0 0 0 1 0 3 0 0)
Get.Events <- function(z) {
  tmp <- rle(z)
  result <- c(-1)
  for (i in 1:length(tmp$lengths)) {
    if (tmp$values[i]) {
      tmp2 <- c(tmp$lengths[i], rep(0, tmp$lengths[i] - 1))
      result <- c(result, tmp2)
    }
    else {
      tmp2 <- c(rep(0, tmp$lengths[i]))
      result <- c(result, tmp2)
    }
  }
  result[-1]
}
Get.Events.And.Intensities <- function(z, data) {
  z <- Get.Events(z)
  max.inten <- rep(0, length(z))
  min.inten <- rep(0, length(z))
  sum.inten <- rep(0, length(z))
  avg.inten <- rep(0, length(z))
  
  indices <- (1:length(z))[z > 0]
  for (i in indices) {
    tmp2 <- data[i:(i + (z[i] - 1))]
    max.inten[i] <- max(tmp2)
    min.inten[i] <- min(tmp2)
    sum.inten[i] <- sum(tmp2)
    avg.inten[i] <- mean(tmp2)
  }
  result <- data.frame(z, min.inten, max.inten, sum.inten, avg.inten)
  names(result) <- c("FeedingEvent",
                     "MinIntensity",
                     "MaxIntensity",
                     "SumIntensity",
                     "MeanIntensity")
  result
}

## This function will take a TRUE/FALSE vector, assumed to be minthresholded lick data
## and it will "bridge" runs of FALSE of less than 'thresh' entries with TRUE
## e.g. c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE) -> c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE)
## with thresh <= 3
Link.Events <- function(z, thresh) {
  tmp <- rle(z)
  result <- c(FALSE)
  for (i in 1:length(tmp$lengths)) {
    if (tmp$values[i]) {
      tmp2 <- rep(TRUE, tmp$lengths[i])
      result <- c(result, tmp2)
    }
    else {
      if (tmp$lengths[i] > thresh) {
        tmp2 <- rep(FALSE, tmp$lengths[i])
        result <- c(result, tmp2)
      }
      else {
        tmp2 <- rep(TRUE, tmp$lengths[i])
        result <- c(result, tmp2)
      }
    }
  }
  result[-1]
}

# This function replaces continuing events with zero and make the first event of that
# episode equal to its duration.
## c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE) -> (0 0 2 0 0 1 0 0 0)
Get.Intervals <- function(z) {
  tmp <- rle(z)
  result <- c(-1)
  for (i in 1:length(tmp$lengths)) {
    if (!tmp$values[i]) {
      tmp2 <- c(tmp$lengths[i], rep(0, tmp$lengths[i] - 1))
      result <- c(result, tmp2)
    }
    else {
      tmp2 <- c(rep(0, tmp$lengths[i]))
      result <- c(result, tmp2)
    }
  }
  result[-1]
}
mySEM <- function(x) {
  if (is.factor(x)) {
    tmp <- unique(as.character(x))
  }
  else{
    x <- x[!is.na(x)]
    tmp <- sqrt(var(x) / length(x))
  }
  tmp
}
GetTreatmentForChamber <- function(dfmNum, chamberNum, expdesign) {
  tmp <- subset(expdesign, DFM == dfmNum)
  tmp <- subset(tmp, Chamber == chamberNum)
  if (nrow(tmp) != 1)
    return("None")
  else
    return(as.character(tmp$Treatment))
}

GetExpDesignForChamber <- function(dfmNum, chamberNum, expdesign) {
  tmp <- subset(expdesign, DFM == dfmNum)
  tmp <- subset(tmp, Chamber == chamberNum)
  if (nrow(tmp) != 1)
    return(NA)
  else
    return(tmp)
}

gt <- function(df, expDesign) {
  return("hi")
}
GetTreatmentForRow <- function(df, expdesign) {
  tmp <- subset(expdesign, DFM == df["DFM"] & Chamber == df["Chamber"])
  if (nrow(tmp) != 1)
    return("None")
  else
    return(as.character(tmp$Treatment))
}
## Need to fix treatment assignments in single and choice experiments!!
AppendTreatmentonResultsFrame <- function(results, expdesign) {
  isChamberThere <- "Chamber" %in% names(results)
  if (isChamberThere == FALSE)
    stop("Need a chamber to assign treatment.")
  Treatment <- rep(NA, nrow(results))
  for (i in 1:nrow(results)) {
    Treatment[i] <- GetTreatmentForChamber(results$DFM[i], results$Chamber[i], expdesign)
  }
  n <- names(results)
  n <- c("Treatment", n)
  results <- cbind(Treatment, results)
  names(results) <- n
  results$Treatment <- factor(results$Treatment)
  results
}


multiplot <- function(...,
                      plotlist = NULL,
                      file,
                      cols = 1,
                      layout = NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol = cols,
                     nrow = ceiling(numPlots / cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]],
            vp = viewport(
              layout.pos.row = matchidx$row,
              layout.pos.col = matchidx$col
            ))
    }
  }
}

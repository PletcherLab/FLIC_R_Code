args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript scripts/generate_golden.R <dfm_id> <out_dir>")
}

dfm_id <- as.numeric(args[1])
out_dir <- args[2]

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## DFM.R uses `mixedsort()` for DFM3 multi-file ordering.
suppressMessages(library(gtools))

source("DFM.R")
source("QC.R")

## QC.R in this repo currently allocates a 4-col matrix but assigns 5 values.
## Override here (without editing repo R code) so golden generation can run.
CheckForSimultaneousFeeding.DFM <- function(dfm){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-well chambers only")
  lick.matrix<-matrix(rep(NA,30),ncol=5)
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

## Use the standard two-well parameters by default, matching the Python tests.
p <- ParametersClass.TwoWell()

dfm <- DFMClass(dfm_id, p, range = c(0, 0))

## Feeding summary (two-well)
summ <- Feeding.Summary.TwoWell(dfm, range = c(0, 0), TransformLicks = TRUE)
write.csv(summ, file = file.path(out_dir, paste0("feeding_summary_DFM", dfm_id, ".csv")), row.names = FALSE)

## Binned feeding summary (minimal port of BinnedFeeding.Summary.DFM)
binsize.min <- 60
range <- c(0, 0)
TransformLicks <- TRUE

if (sum(range) != 0) {
  m.min <- range[1]
  m.max <- range[2]
} else {
  m.min <- 0
  m.max <- max(dfm$RawData$Minutes)
}
if (m.min > m.max) {
  m.max <- m.min + 1
}

y <- seq(m.min, m.max, by = binsize.min)
if (y[length(y)] < m.max) {
  y <- c(y, m.max)
}

tmpMatrix <- cbind(y[-length(y)], y[-1])
intervals <- cut(y + 0.000001, y, include.lowest = TRUE, dig.lab = 8)
intervals <- intervals[-length(intervals)]

bsumm <- Feeding.Summary.TwoWell(dfm, range = tmpMatrix[1, ], TransformLicks = TransformLicks)
Interval <- rep(intervals[1], nrow(bsumm))
Minutes <- rep(mean(tmpMatrix[1, ]), nrow(bsumm))
bsumm <- data.frame(Interval, Minutes, bsumm)
for (i in 2:nrow(tmpMatrix)) {
  tmp <- Feeding.Summary.TwoWell(dfm, range = tmpMatrix[i, ], TransformLicks = TransformLicks)
  Interval <- rep(intervals[i], nrow(tmp))
  Minutes <- rep(mean(tmpMatrix[i, ]), nrow(tmp))
  tmp <- data.frame(Interval, Minutes, tmp)
  bsumm <- rbind(bsumm, tmp)
}
write.csv(bsumm, file = file.path(out_dir, paste0("binned_feeding_summary_60min_DFM", dfm_id, ".csv")), row.names = FALSE)


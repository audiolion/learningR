library("ggplot2")
source("~/[BIOL230] Bioinformatics Languages (R)/lab6/lab06_Castner_Ryan1.R")

someWrapperFunction <- function(){
  helaData2 <- list.files(path="~/helaData2", full.names=T, recursive=FALSE)
  results <- lapply(helaData2, processFile, window=41)
  
  rbindResults <- do.call(rbind, results)
  
  binnedResultsA <- rbindResults[1:41,]
  binnedResultsB <- rbindResults[42:81,]
  binnedResultsC <- rbindResults[82:121,]
  binnedResultsD <- rbindResults[122:161,]
  binnedResultsE <- rbindResults[162:200,]
  
  avgA <- colMeans(binnedResultsA)
  avgB <- colMeans(binnedResultsB)
  avgC <- colMeans(binnedResultsC)
  avgD <- colMeans(binnedResultsD)
  avgE <- colMeans(binnedResultsE)
  
  
  avgs <- cbind.data.frame(avgA, avgB, avgC, avgD, avgE)
  
  names(avgs) <- LETTERS[1:5]
  
  return(avgs)
}

df <- someWrapperFunction()

ggbox <- ggplot(df) + geom_boxplot(aes(x=1,y=df[,1]))
          + geom_boxplot(aes(x=1,y=df[,2])) + geom_boxplot(aes(x=1,y=df[,3])) + geom_boxplot(df[,4])
          + geom_boxplot(aes(x=1,y=df[,5]))
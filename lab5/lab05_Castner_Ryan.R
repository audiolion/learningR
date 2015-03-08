movAvg <- function(inVec, window){
  stopifnot(window > 0, length(window) < length(inVec))
  outVec <- inVec
  if(window %% 2 == 0){
    for(i in inVec){
      if(i - window < 1 | i + window > length(inVec)){
        shortenedWindow <- i
        outVec[i] = mean(inVec[inVec[i]-(shortenedWindow/2):inVec[i]+
                                 (shortenedWindow/2)-1])
      }else{
        outVec[i] = mean(inVec[inVec[i]-(window/2):inVec[i]+(window/2)-1])
      }
    }
  }else{
    for(i in inVec){
      if(i - window < 1 | i + window > length(inVec)){
        shortenedWindow <- i
        outVec[i] = mean(inVec[inVec[i]-(shortenedWindow/2):inVec[i]
                              + (shortenedWindow/2)-1])
      }else{
        outVec[i] = mean(inVec[floor(inVec[i]-(window/2)):floor(inVec[i]+(window/2))])
      }
    }
  }
  return(outVec)
}

processFile <- function(fileName, window){
  df <- read.csv(file=fileName,sep="\t")
  movavg <- movAvg(df[,2], window)
  return(movavg)
}

lineGraph <- function(df, fileName){
  yAxisMin <- min(df[,1:ncol(df)])
  yAxisMax <- max(df[,1:ncol(df)])
  fileNamePng <- paste(fileName, ".png", sep="")
  png(fileNamePng)
  plot.ts(df, plot.type=c("single"), main=fileName, col=rainbow(ncol(df)), ylim=c(yAxisMin, yAxisMax))
  dev.off()
}
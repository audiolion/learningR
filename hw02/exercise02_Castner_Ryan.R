getmonitor <- function(id, directory, summarize = F){
  #fileNames <- dir(directory)
  fullFilePaths <- list.files(path=directory, full.names=T, recursive=F)
  monitorData <- read.csv(file=fullFilePaths[id])
  if(summarize == T){
    print(summary(monitorData, ))
  }
}
getmonitor(1, "specData", TRUE)
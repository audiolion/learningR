#
# Function computes the correlation between sulfate particulate matter and
# nitrate particulate matter levels in the air in the US taken by different
# monitors.
#
# author: Ryan Castner rrc9704@rit.edu

corr <- function(directory, threshold = 0){
  #
  # Function computes correlation of nitrate and sulfate data out of all the
  # monitors in the supplied directory that have readings (complete obs)
  # above a certain threshold. Function returns a vector containing correlation
  # data for each monitor meeting this condition, appended to the vector after
  # it is computed.
  #
  # Args:
  #   directory - the file path of the specdata
  #   threshold - the number of complete obs needed to consider a monitors data
  #
  # Returns:
  #   corrData - a vector containing correlation data results from monitors
  
  fullFilePaths <- list.files(path=directory, full.names=T, recursive=F)
  corrData <- vector()
  for(i in 1:length(fullFilePaths)){
    monitorData <- read.csv(file=fullFilePaths[i])
    nobs <- sum(!is.na(monitorData["sulfate"]) & !is.na(monitorData["nitrate"]))
    if(nobs >= threshold){
      corr <- cor(monitorData["sulfate"], monitorData["nitrate"], use="complete.obs")
      corrData <- append(corrData, corr)
    }
  }
  return(corrData)
}
cr <- corr("specdata", 5000)
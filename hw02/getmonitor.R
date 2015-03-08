#
# Provides a function to get monitor data
#
# author: Ryan Castner rrc9704@rit.edu

getmonitor <- function(id, directory, summarize = F){
  #
  # Function retrieves monitor data about particulate matter in the US from a
  # given directory and supplied file ID, producing a summary of the data if
  # the summarize true arg is supplied
  #
  # Args:
  #   id - the id number of the monitor file
  #   directory - the directory in which the file is located
  #   summarize - boolean, if true prints a summary of the data
  #
  # Returns:
  #   None
  fullFilePaths <- list.files(path=directory, full.names=T, recursive=F)
  monitorData <- read.csv(file=fullFilePaths[id])
  if(summarize == T){
    print(summary(monitorData))
  }
}
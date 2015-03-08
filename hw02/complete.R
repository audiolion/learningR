#
# Function returns how many complete cases of data exist in the data set of
# particulate matter in the air in the US.
#
# author: Ryan Castner rrc9704@rit.edu

complete <- function(directory, id = 1:332){
  #
  # Function computes how many complete cases exist in the supplied data set
  # and for each file examined, inserts the file ID and number of complete case
  # into a data frame which is then returned. A complete case is specified as
  # having no NA values for sulfate or nitrate readings
  #
  # Args:
  #   directory - the file path for specdata
  #   id - a vector containing numbers between 1 and 332 of files to examine
  #
  # Returns:
  #   df - a data frame containing the specified files number of complete cases
  
  fullFilePaths <- list.files(path=directory, full.names=T, recursive=F)
  df <- data.frame(id = numeric(length(id)), nobs = numeric(length(id)))
  colnames(df) <- c("id","nobs")
  index <- 1
  for(i in id){
    monitorData <- read.csv(file=fullFilePaths[i])
    # sum number of complete case observations
    nobs <- sum(!is.na(monitorData["sulfate"]) & !is.na(monitorData["nitrate"]))
    # assign the data to the dataframe with the file ID
    df$id[index] <- i
    df$nobs[index] <- nobs
    index <- index+1
  }
  
  return(df)
}
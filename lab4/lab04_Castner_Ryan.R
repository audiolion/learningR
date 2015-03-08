#
# Script implements a couple different ways to parse through a directory
# of files and return that information in data frame or list format.
#
# author: Ryan Castner rrc9704@rit.edu

readFolderDF1 <- function(folderName){
  #
  # Function takes as input a folder name, pulls names of all file names out
  # of that folder and parses them into a premade data frame using cbind
  #
  # Args:
  #   folderName - the file path of the directory
  #
  # Returns:
  #   df - a data frame containing the parsed files
  
  files <- list.files(path=folderName, full.names=T, recursive=FALSE)
  df <- data.frame(matrix(ncol=0, nrow=2001))
  pb <- txtProgressBar(1, length(files), style=3)
  for(i in 1:length(files)){
    setTxtProgressBar(pb, i)
    if(i == 1){
      scans <- scan(file=files[i], quiet=T)
      scans <- scans[c(F,T)]
      df <- cbind(scans)
    }else{
      scans <- append(scans, scan(file=files[i],quiet=T))
      scans <- scans[c(F,T)]
      df <- cbind(scans)
    }
  }
  close(pb)
  return(df)
}

readFolderDF2 <- function(folderName){
  #
  # Function takes as input a folder name, pulls names of all file names out
  # of that folder and parses them into a dataframe of column size equal to
  # the number of files in the directory
  #
  # Args:
  #   folderName - the file path of the directory
  #
  # Returns:
  #   df - a data frame containing the parsed files
  
  files <- list.files(path=folderName, full.names=T, recursive=FALSE)
  df <- data.frame(matrix(ncol=length(files)))
  pb <- txtProgressBar(1, length(files), style=3)
  for(i in 1:length(files)){
    setTxtProgressBar(pb, i)
    if(i == 1){
      scans <- scan(file=files[i], quiet=T)
      scans <- scans[c(F,T)]
      df[,i] <- rbind(scans)
    }else{
      scans <- append(scans, scan(file=files[i],quiet=T))
      scans <- scans[c(F,T)]
      df[,i] <- rbind(scans)
    }
  }
  close(pb)
  return(df)
}

readFolderList <- function(folderName){
  #
  # Function takes as input a folder name, pulls names of all file names out
  # of that folder and parses them into an empty list
  #
  # Args:
  #   folderName - the file path of the directory
  #
  # Returns:
  #   lst - a list containing the parsed files
  
  files <- list.files(path=folderName, full.names=T, recursive=FALSE)
  lst <- list()
  pb <- txtProgressBar(1, length(files), style=3)
  for(i in 1:length(files)){
    setTxtProgressBar(pb, i)
    if(i == 1){
      scans <- scan(file=files[i], quiet=T)
      scans <- scans[c(F,T)]
      lst <- c(lst, scans)
    }else{
      scans <- append(scans, scan(file=files[i],quiet=T))
      scans <- scans[c(F,T)]
      lst <- c(lst, scans)
    }
  }
  close(pb)
  return(df)
}
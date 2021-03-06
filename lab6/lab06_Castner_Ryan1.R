#
# Script defines 3 functions, movAvg, processFile, and lineGraph.
# movAvg creates a moving average of an input vector given a window size.
# processFile utilizes movAvg function and ensures it is passed proper input
#  from the helaData dataset
# lineGraph produces a colored plot of a dataframe, plotting pieces of a data
#  frame against each other
#
# author: Ryan Castner rrc9704@rit.edu
#

movAvg <- function(inVec, window){
  #
  # Function defines a moving average for a vector. By defining a window size
  # the function averages the mean along the vector by the window size, with
  # the window being shortened at the ends of the vector as appropriate. This
  # will shift the average of the vector slightly.
  #
  # Args:
  #   inVec - input vector
  #   window - the window size
  #
  # Returns:
  #   outVec - the output vector of each average based on window size
  
  # check for valid window size
  stopifnot(window > 0, length(window) < length(inVec))
  outVec <- inVec
  
  # check if window is even or odd to determine the implementation of the 
  # moving average
  if(window %% 2 == 0){
    # iterate through vector and compute average based on a window of the vec
    for(i in 1:length(inVec)){
      # case of shortened window due to falling below length 1
      if(i - window < 1){
        shortenedWindow <- i
        outVec[i] = mean(inVec[(i-floor(shortenedWindow/2)):(i+floor(window/2)-1)])
      }else if(i + window > length(inVec)){ # case of window passing vector len
        shortenedWindow <- length(inVec)-i
        outVec[i] = mean(inVec[(i-floor(window/2)):(i+floor(shortenedWindow/2)-1)])
      }else{  # default case
        outVec[i] = mean(inVec[(i-(window/2)):(i+(window/2)-1)])
      }
    }
  }else{
    # odd window case, utilizes floor function in default case to ensure
    # an integer index
    for(i in 1:length(inVec)){
      if(i - window < 1){
        shortenedWindow <- i
        outVec[i] = mean(inVec[(i-floor(shortenedWindow/2)):(i+floor(window/2))])
      }else if(i + window > length(inVec)){
        shortenedWindow <- length(inVec)-i+1
        outVec[i] = mean(inVec[(i-floor(window/2)):(i+floor(shortenedWindow/2))])
      }else{
        outVec[i] = mean(inVec[(i-floor(window/2)):(i+floor(window/2))])
      }
    }
  }
  return(outVec)
}

processFile <- function(fileName, window){
  #
  # Function is a wrapper of sorts for movAvg that sets up the input for the
  # movAvg function so it can be called based on the helaData.zip data set
  #
  # Args:
  #   fileName - name of the file in default directory
  #   window - window size to pass to movAvg
  #
  # Returns:
  #   movavg - the vector of window averages means
  
  df <- read.csv(file=fileName,sep="\t")
  movavg <- movAvg(df[,2], window)
  return(movavg)
}

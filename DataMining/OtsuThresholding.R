# 
# Program analyzes two data sets and uses Otsu's method to
# cluster the data into two parts based on an optimal threshold
# that minimizes variance. The program plots results and prints
# statistics to the console window.
#
# usage: use source() to run. Data sets are located and named as:
# ~/UNCLASSIFIED_Speed_Observations_for_128_vehicles.csv
# ~/Mystery_Data.csv
# if they are not located in your default ~ dir the program will not work
#
# author: Ryan Castner - castner.rr@gmail.com


# Read in data, quantize data based on bins of 2 from 38 to 80
# then plot a histogram of this data
mydata <- read.csv("~/UNCLASSIFIED_Speed_Observations_for_128_vehicles.csv",
                   head=FALSE, sep=",")
binWidth <- 2
x <- sort(mydata$V1, decreasing=FALSE)
x <- as.vector(x)
hist(mydata$V1, breaks=seq(38,80,by=binWidth), main='Histogram of Vehicle Speed',
     xlab='Speed (mph) quantized by 2 mph increments')

# create two global vectors for the final plot of Threshold vs. Mixed Variance
mixedVarianceData <- c()
thresholdData <- c()
cat("-----------Vehicle Speeds Data Stats-----------\n");
otsuCluster <- function(x){
  # Computes best threshold based on Otsu's method which minimizes variance
  # Prints results to console and adds line to histogram of best threshold.
  #
  # Args:
  #   x: A vector containing data you want to cluster
  #       Assumed to be bimodal in nature.
  #
  # Returns:
  #   No return
  
  # initialize variance and threshold
  bestVariance <- Inf
  bestThreshold <- Inf
  # define threshold values from 38-80 by 2's
  threshold <- seq(38,80,by=2)
  # define size to improve readability
  size <- length(x)
  
  # Iterate through all possible threshold values
  for(i in 1:length(threshold)){
    # sum how many values less than or equal to threshold
    wtUnder <- sum(x <= threshold[i])
    # sum how many values greater than threshold
    wtOver <- sum(x > threshold[i])
    # get values that are under or equal to threshold
    temp <- x[1:wtUnder]
    # calculate variance of these values
    varUnder <- var(temp)
    # get values above threshold
    temp <- x[(wtUnder+1):size]
    # calculate variance of these values
    varOver <- var(temp)
    # calculate mixed variance
    mixVar <- ((wtUnder/size) * varUnder + (wtOver/size) * varOver);
    
    # save threshold data for final plot
    thresholdData <<- append(thresholdData, threshold[i])
    # check if variance is NA, happens when wtUnder or wtOver is 0 or 1 point
    if(is.na(mixVar) != TRUE){
      # save mixed variance data for final plot
      mixedVarianceData <<- c(mixedVarianceData, mixVar)
      # if we have a better mixed variance than our current best
      if(mixVar < bestVariance){
        # assign the new best variance and threshold values
        bestVariance <- mixVar
        bestThreshold <- threshold[i]
      }    
    }else{
      # if variance was NA we set it to be 0 to be later cleaned out of data
      mixedVarianceData <<- c(mixedVarianceData, 0)
    }   
  }
  cat("Best threshhold", bestThreshold, "best Variance:", bestVariance,"\n")
  abline(v=bestThreshold,col=2, lwd=3)
}

otsuCluster(x)

# Start of mystery data set anaylsis
cat("\n\n-----------Mystery Data Stats-----------\n")
# Read in data, generate histogram
mystData <- read.csv("~/Mystery_Data.csv", head=TRUE, sep=',')
y <- as.vector(mystData$Measures..Mystery.Units.)
hist(mystData$Measures..Mystery.Units., main='Histogram of Mystery Data',
     xlab='Mystery Units')

# function to find the mode
Mode <- function(y){
  # Computes the mode of a vector of numbers or characters
  #
  # Args:
  #   x: The vector whose mode is to be calculated
  #
  # Returns:
  #   the mode value of x
  
  # generate unique values to check against
  uniqueYs <- unique(y)
  # match unique values to vector and tally them, return the max tallied elemnt
  uniqueYs[which.max(tabulate(match(y, uniqueYs)))]
}

# Print mean, mode and median of data
cat("Mean:",mean(y, na.rm=TRUE),"Mode:",Mode(y), "Median",
    median(y, na.rm=TRUE),"\n")
# remove the last element of the data set
cat("Last element removed:",y[length(y)],"\n")
y <- y[1:(length(y)-1)]
# Print mean, mode and median of modified data set
cat("New Mean:",mean(y, na.rm=TRUE),"New Mode:",Mode(y), "New Median",
    median(y, na.rm=TRUE),"\n")

otsuCluster2 <- function(y){
  # Computes best threshold based on Otsu's method which minimizes variance
  # Prints results to console and adds line to histogram of best threshold.
  #
  # Args:
  #   y: A vector containing data you want to cluster
  #       Assumed to be bimodal in nature.
  #
  # Returns:
  #   No return
  
  # Initialize variables
  bestVariance <- Inf
  bestThreshold <- Inf
  # set threshold possible values from min of vector to max of vector
  threshold <- seq(min(y),max(y))
  # set size for readability
  size <- length(y)
  
  # Iterate through all possible thresholds
  for(i in 1:length(threshold)){
    # sum how many values are under or equal to threshold value
    wtUnder <- sum(y <= threshold[i])
    # sum how many values are greater than threshold value
    wtOver <- sum(y > threshold[i])
    # pull out values under or equal to threshold
    temp <- y[1:wtUnder]
    # calculate their variance
    varUnder <- var(temp)
    # pull out values above threshold
    temp <- y[(wtUnder+1):size]
    # calculate their variance
    varOver <- var(temp)
    # calculate mixed variance
    mixVar <- ((wtUnder/size) * varUnder + (wtOver/size) * varOver)
    
    # check for NA, occurs when wtUnder or wtOver contain 0 or 1 points
    if(is.na(mixVar) != TRUE){
      # check for new best variance
      if(mixVar < bestVariance){
        # assign new best variance and threshold
        bestVariance <- mixVar;
        bestThreshold <- threshold[i]
      }    
    }  
  }
  # print best threshold and variance
  cat("Best threshhold", bestThreshold, "best Variance:", bestVariance,"\n");
  # add line to current histogram of best threshold
  abline(v=bestThreshold,col=2, lwd=3)
}

otsuCluster2(y);

cleanData <- function(a, b){
  # Removes all zeroes from a vector a and the corresponding data at the
  # same index in vector b.
  #
  # Args:
  #   a: Vector that contains zeroes to be removed
  #   b: Vector that has data points corresponding to the zeroes in vector a
  #
  # Returns:
  #   A list containing both vectors under the names $var and $thresh
  
  # define empty vectors
  c <- c()
  d <- c()
  # Iterate through vector a
  for(i in 1:length(a)){
    # if a zero is there do nothing
    if(a[i] == 0){
    }else{
      # if a zero is not at the current index, add elements corresponding to
      # a and b vectors to c and d vectors
      c <- c(c, a[i])
      d <- c(d, b[i])
    }
  }
  # create a list containing the clean c and d vectors
  dataList <- list("var" = c, "thresh" = d)
  # return the list
  return(dataList)
}

# get the cleaned data
myList <- cleanData(mixedVarianceData, thresholdData)
# plot Threshold vs. Mixed Variance
plot(myList$thresh, myList$var, type="l", col="blue", lwd=3, 
     main='Threshold vs. Mixed Variance of Car Speeds', ylab='Mixed Variance',
     xlab='Threshold')
# emphasize data points on line graph in black
points(myList$thresh, myList$var, col="black", lwd=3)
# 
# Program analyzes a training set of vehicle speed data that has already
# been classified and finds the optimal threshold value that minimizes the
# miscalculation rate and maximizes public trust by choosing the higher speed
# threshold given a tie.
#
# usage: use source() to run. Data set is located and named as:
# ~/CLASSIFIED_TRAINING_SET_FOR_RECKLESS_DRIVERS.csv
#
# if it is not located in your default ~ dir the program will not work
#
# dependences:
# ggplot2 and all of its depedencies
#
# author: Ryan Castner - castner.rr@gmail.com

# must have ggplot2 package and dependencies installed
library(ggplot2)

# read in data
trainSet <- read.csv('~/CLASSIFIED_TRAINING_SET_FOR_RECKLESS_DRIVERS.csv')
# define bin width
binWidth <- 0.5
# define vector of speed data
x <- as.vector(trainSet["SPEED"])
# round data to nearest .5 by multiplying by 2, rounding, then dividing by 2
trainSet["SPEED"] <- (round(x*2)/2)
# order the data by the speed column in ascending order (low to high)
trainSet <- trainSet[order(trainSet$SPEED),]

# create a stacked histogram plot that is quantized by .5 bins, blue shows
# safe driving intent, red shows reckless driving intent, stacked red and blue
# shows overlap where reckless and safe driving intent drivers are going the
# same speed
p1 <- ggplot(trainSet, aes(x=SPEED, fill=RECKLESS==0)) +
  geom_histogram(binwidth=.5, alpha=.55, position="identity") +
  ggtitle("Training Set of Vehicle Speed and Reckless Intent") +
  xlab("Speed (mph)") +
  scale_y_discrete() +
  scale_x_continuous(breaks=seq(38,75,by=2)) +
  scale_fill_discrete(name="Condition",
                      labels=c("Reckless","Safe"))
print(p1)
# vectors for bonus question
miscalculateData <- c()
thresholdData <- c()


otsuMethod <- function(x){
  # Computes best threshold based on Otsu's method which minimizes the rate of
  # miscalculation and prints results to console.
  #
  # Args:
  #   x: A dataframe containing data you want to cluster
  #       Assumed to be bimodal in nature and have columns "SPEED", "RECKLESS"
  #       located at column positions 1 and 2, respectively.
  #
  # Returns:
  #   No return
  
  # initialize best rates
  bestMiscalculationRate <- Inf
  bestThreshold <- Inf
  
  # create threshold from the minimum value of dataset to max by 0.5
  threshold <- seq(x[1,"SPEED"],x[length(x$SPEED),"SPEED"],by=0.5)
  
  # iterate through all threshold values
  for(i in 1:length(threshold)){
    #TP = they are driving above threshold and are driving reckless
    truePositive <- sum(threshold[i] > x["SPEED"] & x["RECKLESS"] == 1)
    #TN = they are driving less than threshold and aren't driving reckless
    trueNegative <- sum(threshold[i] <= x["SPEED"] & x["RECKLESS"] == 0)
    #FN = they are driving less than threshold and are driving reckless
    falseNegative <- sum(threshold[i] <= x["SPEED"] & x["RECKLESS"] == 1)
    #FP = they are driving above threshold and aren't driving reckless
    falsePositive <- sum(threshold[i] > x["SPEED"] & x["RECKLESS"] == 0)
    
    # mcr = 1-(FN+FP)/(TP+TN+FP+FN)
    currentMiscalculationRate <- 1 - (falseNegative + falsePositive)/
      (truePositive + trueNegative + falsePositive + falseNegative)
   
    # assign data for bonus question plot
    miscalculateData <<- append(miscalculateData, currentMiscalculationRate)
    thresholdData <<- append(thresholdData, threshold[i])
    
    # check for new best miscalc rate, or higher threshold with same rate
    if(currentMiscalculationRate <= bestMiscalculationRate){
      bestMiscalculationRate <- currentMiscalculationRate
      bestThreshold <- threshold[i]
    }
  }
  #print results
  cat("Best Threshold:",bestThreshold,"Miscalc Rate:",bestMiscalculationRate)
}

# call method
otsuMethod(trainSet)

# set up data frame for bonus question
df <- data.frame(miscalculateData, thresholdData)
names(df) <- c("MiscalcRate","Threshold")

# plot bonus question miscalc rate vs. threshold values, circling lowest rate
p2 <- ggplot(df, aes(x=MiscalcRate, y=Threshold)) +
        coord_flip() +
        geom_point(aes(colour = MiscalcRate)) + 
        scale_colour_gradient(low="blue") +
        geom_point(data = subset(df, MiscalcRate == min(df$MiscalcRate)), 
                   colour="red", size=7, shape=1)
print(p2)
        
  
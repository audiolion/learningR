#
# Program demonstrates random number generation from a uniform distribution,
# matrix and functon construction, and matrix to data frame conversion.
#
# author: Ryan Castner rrc9704@rit.edu

getRandDF <- function(width, length){
  #
  # Function creates a random data frame of size width and length and fills it
  # with numerical data between two randomly generated values that are
  # themselves between 0 and 100.
  #
  # args:
  #   width - the number of columns of the data frame
  #   length - the number of rows of the data frame
  #
  # returns:
  #   df - a data frame of size width*length filled with random values
  #
  
  # generate two random numbers between 0 and 100 from a uniform distribution
  twoNums <- runif(2, min=0, max=100)
  
  # print the two random values between 0 and 100
  print(twoNums)
  
  # construct a matrix filled with values between twoNums values, again uniform
  A = matrix(
    runif(width*length, min=min(twoNums), max=max(twoNums)),
    nrow=length,
    ncol=width
    )

  #convert matrix to a data frame
  df <- data.frame(A)
  
  # return the data frame
  return(df)
}

# call the function 
df <- getRandDF(5,7)
# print results
print(df)

# Data Frame vs. Matrix
# data frames in R can hold different data types in each column, matrices can't
# when you have the same data type, the choice between df and matrix depends
# on how it is to be used. Matrices are more memory efficient, can have linear
# algebra operations applied to them, and arguably have better formatting. Data
# frames are better as a return value though as they can be passed into most
# generic functions that use data frames so they can deal with any input type
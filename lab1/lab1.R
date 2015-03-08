x <- c(10,16,-3,116,18,22,38,41,-89)	# Defining an arbitrary vector and storing it
cat("Here is a vector:", x, "\n")	# Printing the vector
cat("The same vector reversed:", rev(x), "\n")	# Calling reverse function to reverse the elements
cat("Here is a random permutation of the vector:", sample(x), "\n")	# Sample function produces a psuedorandom permutation
cat("The mean of this vector is:", mean(x), "\n")	# A few more basic function calls on vector
cat("The standard deviation of this vector is:", sd(x), "\n")	
y <- sample(1:6, 10, replace=T)		#psuedorandomly sampled vector of 10 dice rolls
cat("Here is another vector of 10 dice rolls:", y, "\n")	# Print the vector
temp <- table(as.vector(y))	# store vector as table that counts how many of each roll there were

# names(temp) [temp == max(temp)] pulls out the highest roll counts, names(temp) is the first row of the table holding possible
# dice roll values of 1-6, temp == max(temp) specifies a filter to pull out only the highest frequencies of the second row
# as a result we if in 10 rolls, there were 6 1's rolled, it would print the number 1, followed by its roll frequency
cat("The most rolled value(s): ", (names(temp) [temp == max(temp)]), ", repeated ", strtrim(max(as.vector(temp)),1), " times.\n", sep = "")

# This function uses a binomial distribution function to find the probability of rolling the highest frequency roll
# pbinom returns the probability of P(x <= Frequency), but we want P(x = Frequency) so we subtract 1 from the highest frequency
# and take the pbinom again to get P(x <= Frequency-1) and subtract that from P(x <= Frequency) to get just P(x = Frequency)
cat("The probability of throwing exactly", strtrim(max(as.vector(temp)),1))
cat(" ", which.max(as.vector(temp)), "'s in 10 throws is ", sep = "")
cat((pbinom(as.vector(max(as.vector(temp))), 10, .167777)-pbinom(as.vector(max(as.vector(temp-1))), 10, .167777))*100, "%.", sep = "")

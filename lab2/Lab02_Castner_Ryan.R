#
# Program creates some vectors and analyzes certain nuances of R and uses the
# plot function and saves the data to the default directory
#
# author: Ryan Castner rrc9704@rit.edu
#

# Create a vector of values 1-50 and then 100 minus integers 1-50.
A <- c(seq(1,50), (100-(1:50)))

B <- c(sqrt(A)^2-A)
# B assignment produces nonzeros.
# The nonzeros are because of precision errors
# Even though mathematically they should be zero, the program first evaluates
# the squareroot which is slightly imprecise and then squares it which creates
# even more precision issues. A tolerance should be used here to eliminate this

C <- c(sqrt(A^2)-A)
# C assignment produces zeros.
# This is because there are no precision errors by squaring an Integer
# value and then taking the square root of it. So we get the expected
# mathematical result of A-A.

D <- A[seq(3,length(A), by=6)]

# These values are zero because the same values in the B array
# were zero within a tolerance.
E <- A[B==0]


# Plot A as a scatter plot, save it
plot(A, main="Scatterplot of A", xlab="Integer Data", pch=19)
dev.copy(png, 'scatterplotA.png')
dev.off()

# Plot A as a line plot, save it
plot(A, main="Lineplot of A", xlab="Integer Data", type="l", lwd="2")
dev.copy(png, "lineplotA.png")
dev.off()

# Plot A against B as a scatter plot, save it
plot(A, B, main="Scatterplot of A vs. B", xlab="A", ylab="B")
polygon(A[1:10],B[1:10], col='blue')
dev.copy(png, "scatterplotAvsB.png")
dev.off()
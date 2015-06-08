# Function 'computeApproximateGradient'
# Killian Martin--Horgassan
# 08-06-2015

# Computes the approximate gradient of the function
# t -> Empirical price of the stock at t


computeApproximateGradient <- function(x) {
	# By convention we take a 0 for the gradient at 
	# initial time. Should be NA.
	N <- length(x)
	RESULT <- rep(0,N)
	RESULT[2:N] <- (x[2:N] - x[1:N-1])/x[1:N-1]
	output <- RESULT
}
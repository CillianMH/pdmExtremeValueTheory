# Function 'netReturns'
# Killian Martin--Horgassan
# 17-03-2015

# Extracts the net returns for a vector of stock prices

netReturns <- function(x) {
	LENGTH <- length(x)
	RESULT <- rep(0,LENGTH-1)
	for (i in 1:LENGTH-1) {
		RESULT[i] <- (x[i+1]-x[i])/x[i]
		}
	output <- RESULT
}
# Function 'logGrossReturns'
# Killian Martin--Horgassan
# 17-03-2015

# Extracts the log gross returns for a vector of stock prices

logGrossReturns <- function(x) {
	LENGTH <- length(x)
	RESULT <- rep(0,LENGTH-1)
	for (i in 1:LENGTH-1) {
		RESULT[i] <- log(x[i+1]/x[i])
		}
	output <- RESULT
}
# Function 'meanAndVarianceDF'
# Killian Martin--Horgassan
# 01-06-2015

# Computes the mean and variance for the supposedly normal 
# log-gross returns (Black-Scholes model) from the empirical 
# data on the 5 stocks

meanAndVarianceDF <- function(df) {
	stock1 <- meanAndVariance(df$V1)
    stock2 <- meanAndVariance(df$V2)
	stock3 <- meanAndVariance(df$V3)
	stock4 <- meanAndVariance(df$V4)
	stock5 <- meanAndVariance(df$V5)

	Mean <- c(stock1[1], stock2[1], stock3[1], stock4[1], stock4[1])
	Variance <- c(stock1[2], stock2[2], stock3[2], stock4[2], stock4[2])
	
	output <- data.frame(Mean, Variance)
}
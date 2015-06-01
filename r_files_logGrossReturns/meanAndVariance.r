# Function 'meanAndVariance'
# Killian Martin--Horgassan
# 01-06-2015

# Computes the mean and variance for the supposedly normal 
# log-gross returns (Black-Scholes model) from the empirical 
# data on the stock

meanAndVariance <- function(x) {
	RESULT <- rep(0,2)
	mu_emp <- mean(x)
	sigma2_emp <- var(x)
	RESULT <- c(mu_emp + 0.5*sigma2_emp, sigma2_emp)	
	output <- RESULT
}
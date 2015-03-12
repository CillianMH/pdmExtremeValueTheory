# Function 'ExtractMaxima'
# Killian Martin--Horgassan
# 10-03-2015

# Extract the successive maxima from a numeric vector

ExtractMaxima <- function(x) {
	LENGTH <- length(x)
	RESULT <- rep(0,LENGTH)
	RESULT[1] <- x[1]
	for (i in 2:LENGTH) {
		RESULT[i] <- max(x[i],RESULT[i-1])
		}
	output <- RESULT
}
# Function 'replaceExtremes'
# Killian Martin--Horgassan
# 08-06-2015

# Replaces values that are too high in a series

replaceExtremes <- function(x, threshold, replacement) {
	aux <- x > threshold
	RESULT <- x
	RESULT[aux] <- rep(replacement, length(RESULT[aux]))	
	output <- RESULT
}
# Function 'firstElement'
# Killian Martin--Horgassan
# 08-06-2015

# Takes a logical vector and find the first FALSE element 
# in it, returns the index of that element


firstElement <- function(x) {
	index <- 1
	N <- length(x)
	while(x[index]) {
		index <- index +1
	}
	output <- index
}
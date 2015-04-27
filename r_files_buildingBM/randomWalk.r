# Random Walk for Brownian Motion
# S_0 = 0
# For all n >= 1, S_n = Epsilons_1 + ... Epsilons_n
# ie S_n = S_(n-1) + Epsilons_n
# 23-04-2015
# Killian Martin--Horgassan

randomWalk = function(n, Epsilons) {
	
	# The vector of the Epsilon_i, they are uniformly distributed on {-1,+1}
	vect <- Epsilons
	
	output <- rep(0,n+1)
	
	for (i in 2:(n+1)) {
		output[i] <- output[i-1]+vect[i-1]
	}
	
	return(output)
}

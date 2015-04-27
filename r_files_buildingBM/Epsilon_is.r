# The sequence of the epsilons in a random walk
# A vector of size n, the elements are generated
# at random : +1 with probability 0.5 and -1 with
# probability 0.5
# 23-04-2015
# Killian Martin--Horgassan

Epsilon_is = function(n) {
	
	# The vector of the Epsilon_is, they are uniformly distributed on {-1,+1}
	vect <- sample(x = c(-1,1), size = n, replace = TRUE, prob = c(0.5,0.5))
	
	return(vect)
}

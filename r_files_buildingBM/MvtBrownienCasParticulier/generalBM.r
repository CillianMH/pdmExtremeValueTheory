# A general Brownian motion with mean non necessarily 0
# and non-necessarily unit variance.
# 23-04-2015
# Killian Martin--Horgassan

generalBM = function(mu, sigma,h) {
	
	# Number of time steps
	t <- ceiling(1/h)
	
	# Generating a t-uplet Gaussian vector and taking the sum
	vect <- cumsum(rnorm(t, mu*h, sigma*h))
	
	return(vect)
}
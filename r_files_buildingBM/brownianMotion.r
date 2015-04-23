# A approximate construction (you dn't get continuum-s with a computer)
# of the standard Brownian Motion from random walks
# 23-04-2015
# Killian Martin--Horgassan

# Arguments :
# - n : an integer, will be very large to get a limit behaviour
# - t : a nonnegative real number, as we want to check the distribution
#       of B^(n)(t) = brokenLine(nt)/sqrt(n)

brownianMotion = function(n,t) {
	
	return(brokenLine(n*t)/sqrt(n))
}
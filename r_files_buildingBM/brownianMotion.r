# A approximate construction (you dn't get continuum-s with a computer)
# of the standard Brownian Motion from random walks
# 23-04-2015
# Killian Martin--Horgassan

# Arguments :
# - t : a nonnegative real number, as we want to check the distribution
#       of B^(n)(t) = brokenLine(nt)/sqrt(n)
# - h : increment size

brownianMotion = function(t,h) {
	
	return(brokenLine((1/h)*t)/sqrt(1/h))
}
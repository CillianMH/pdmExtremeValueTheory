# Objective function
# 18-03-2015
# Killian Martin--Horgassan

objectiveFunction = function(x,n) {
	output <- log(x) + x^2/2+log(sqrt(2*pi))-log(n)
}
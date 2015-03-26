# Objective function
# 18-03-2015
# Killian Martin--Horgassan

objectiveFunction = function(x,n,gamma,x0) {
	output <- x - gamma*tan(pi/2-pi/n) - x0
}
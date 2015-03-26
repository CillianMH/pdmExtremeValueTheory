# Newton-Raphson algorithm
# 18-03-2015
# Killian Martin--Horgassan

newtonRaphson = function(x0, n, func, funcPrime, epsilon, numIterMax,pos,gamma) {
	xprec <- x0
	xcur <- x0 - (func(x0,n,gamma,pos)/funcPrime(x0))
	res <- abs(xprec-xcur)
	count <- 1
	
	while ((count < numIterMax) && (res > epsilon)) {
		xcur <- xprec- (func(xprec,n,gamma,pos)/funcPrime(xprec))
		res <- abs(xprec-xcur)
		count <- count + 1
	}
	
	output <- xcur
	
}
# Applying the Newton-Raphson algorithm to the problem
# 18-03-2015
# Killian Martin--Horgassan

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("/Users/kimartin/Desktop/R_files_pdm/Tache3/newtonRaphson.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache3/objectiveFunction.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache3/objectiveFunctionPrime.r")

N = 1000
x0 = sqrt(2*log(N))
func = objectiveFunction
funcPrime = objectiveFunctionPrime
epsilon = 10^-6
nIterMax = 100

X = rep(0,N)
for (i in 1:N) {
	X[i] = abs(newtonRaphson(x0, i, func, funcPrime, epsilon, nIterMax)-x0)/log(i)
}

quartz()
title <- 'Rate of convergence of the correction'
xlabel <- 'Time step'
ylabel <- expression(abs(Corr)/log(n))
plot(X, xlab = xlabel, ylab = ylabel, main = title, las = 2, type = 'l')

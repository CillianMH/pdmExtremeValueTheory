# Applying the Newton-Raphson algorithm to the problem
# 18-03-2015
# Killian Martin--Horgassan

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("/Users/kimartin/Desktop/R_files_pdm/Tache4/newtonRaphson.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache4/objectiveFunction.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache4/objectiveFunctionPrime.r")

N = 1000
gamma = 1
x0 = 0
func = objectiveFunction
funcPrime = objectiveFunctionPrime
epsilon = 10^-6
nIterMax = 100

X = rep(0,N)
for (i in 1:N) {
	xInit = gamma*(1+2*pi*(1/4-1/i)) + x0
	X[i] = abs(newtonRaphson(xInit, i, func, funcPrime, epsilon, nIterMax, x0, gamma)-xInit)/i
}

quartz()
title <- 'Rate of convergence of the correction'
xlabel <- 'Time step'
ylabel <- expression(abs(Corr)/n)
plot(X, xlab = xlabel, ylab = ylabel, main = title, las = 2)
quartz()
plot(X[2:N], xlab = xlabel, ylab = ylabel, main = title, las = 2)

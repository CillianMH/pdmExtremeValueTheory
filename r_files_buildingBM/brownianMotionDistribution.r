# Master Thesis Project - Extreme Value Theory
# Looking into the distribution of B^(n)[t]
# Killian Martin--Horgassan
# 23-04-2015

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("/Users/kimartin/Desktop/R_files_pdm/Tache5/randomWalk.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache5/Epsilon_is.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache5/brokenLine.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache5/brownianMotion.r")


# Let the user enter the time horizon of the brownian motion
cat("Time step ?")
t <- as.numeric(readline("t :\n"))

# Number of trials
cat("Sample size ?")
N <- as.numeric(readline("N :\n"))

# n must be large for B^(n)[t] to be a good approximation of B[t]
n <- 1000

# Generating the Brownian Motion
BM_vect <- rep(0,N)
for (i in 1:N) {
	BM_vect[i] <- brownianMotion(t,n)
}

# Plot the Brownian Motion
quartz() 

title <- paste("Sample from a distribution", expression(B[t]^(n)), " with t = ", t)

hist(BM_vect, main = "Density plot", freq = FALSE, col = "lightgreen")
curve(dnorm(x, mean = mean(BM_vect), sd = sd(BM_vect)), add = TRUE, col = "darkblue", lwd = 4)
print(var(BM_vect))
curve(dnorm(x, mean = 0, sd = sqrt(t)), add = TRUE, col = "red", lwd = 4)



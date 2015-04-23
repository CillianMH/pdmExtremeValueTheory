# Master Thesis Project - Extreme Value Theory
# Plotting a sample of the Brownian Motions
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

# t itself will be divided in increments
cat("Increment size ?")
h <- as.numeric(readline("h :\n"))

# Number of increments
Nb_incr <- ceiling(t/h)

# n must be very large to get a good approximation
n <- 1000

# Generating the Brownian Motion
BM_vect <- rep(0,Nb_incr)
for (i in 2:Nb_incr) {
	BM_vect[i] <- brownianMotion(t,n)
}

# Plot the Brownian Motion
quartz() 

title <- paste("A brownian motion of time horizon t", t, "with increment h", h, sep = " ")
xlabel <- paste("Time t in", expression(h^-1),"time units", sep= " ")
ylabel <- expression(B[t]^(n))

plot(BM_vect,xlab = xlabel, ylab = ylabel, main = title, type ='l')



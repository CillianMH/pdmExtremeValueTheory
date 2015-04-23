# Master Thesis Project - Extreme Value Theory
# Plotting a sample of the Brownian Motions
# Killian Martin--Horgassan
# 23-04-2015

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("./generalBM.r")

# Let the user enter the time horizon of the brownian motion
cat("Time increment ?")
h <- as.numeric(readline("h :\n"))

cat("Mean ?")
mu <- as.numeric(readline("mu :\n"))

cat("Standard deviation ?")
sigma <- as.numeric(readline("sigma :\n"))

# Generating the Brownian Motion
BM_vect <- generalBM(mu,sigma,h)

# Plot the Brownian Motion
quartz() 

plot(BM_vect, type ='l')
abline(h = 0, col = "red")

# Some Data on the sum of the B(ti)
cat("Mean of sum :\n")
print(mean(BM_vect))
cat("\nVariance of sum :\n")
print(var(BM_vect))

# Now checking the increments respect a few properties of BM
cat("\nTime horizon T =\n")
print(ceiling(1/h))
cat("\nSelect two times a and b, 0 < a < b <=T\n")
a <- as.numeric(readline("Your a :\n"))
b <- as.numeric(readline("Your b :\n"))
cat("\na :\n")
print(a)
cat("\nb\n")
print(b)

Nbtries <- 1000
incr <- rep(0,Nbtries)

for (i in 1:Nbtries) {
	
	# Loop to gather data for a histogram
	# recovering B(a) and B(b)
	vect <- generalBM(mu,sigma,1/b)
	incr[i] <- vect[b] - vect[a]
		
}

hist(incr, main = "Density plot", freq = FALSE, col = "lightgreen")
curve(dnorm(x, mean = mean(incr), sd = sd(incr)), add = TRUE, col = "darkblue", lwd = 4)
curve(dnorm(x, mean = 0, sd = sigma*sqrt(b-a)), add = TRUE, col = "red", lwd = 4)

print(mean(incr))
print(sd(incr))


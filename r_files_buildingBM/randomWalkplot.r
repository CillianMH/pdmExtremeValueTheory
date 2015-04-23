# Master Thesis Project - Extreme Value Theory
# Plotting a random walk at n steps
# Killian Martin--Horgassan
# 23-04-2015

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("/Users/kimartin/Desktop/R_files_pdm/Tache5/randomWalk.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache5/Epsilon_is.r")


# Let the user enter the number of steps to the random walk
cat("How many steps to the random walk ?")
Nb <- as.numeric(readline("n :\n"))

# Get the random Walk and print it if the vector is not too large
Epsilons <- Epsilon_is(Nb)
S <- randomWalk(Nb,Epsilons)
if (Nb <= 25){
	print(S)
} 

# Plot the Random Walk
quartz() 

title <- paste("A random walk of ", Nb, "steps", sep = " ")
xlabel <- "Time steps i"
ylabel <- expression(S[i])

plot(S,xlab = xlabel, ylab = ylabel, main = title, type ='l')



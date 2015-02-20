# Function 'InputIntroductoryTask'
# Killian Martin--Horgassan
# 19-02-2015

# Manages the input for the sample size and distribution chosen
# USED IN script IntroductoryTask.r

InputIntroductoryTask <- function() {
cat("Enter the sample size")
Nb <- as.numeric(readline("Sample size :\n"))
cat("Choose a distribution :\n","- [1] for a gaussian distribution\n",
	"- [2] for an exponential distribution\n",
	"- [3] for a Cauchy distribution\n")
choice_dist <- as.character(readline("Your choice ? :\n"))
if (choice_dist == "1") {
	m  <- as.numeric(readline("\nExpectation ? :\n"))
	s  <- as.numeric(readline("\nStandard deviation ? :\n"))
	DIST <-rnorm(Nb,m,s)
} else if (choice_dist == "2") {
	lambda  <- as.numeric(readline("\nRate ? :\n"))
	DIST <- rexp(Nb,lambda)
} else if (choice_dist == "3") {
	l  <- as.numeric(readline("\nLocation ? :\n"))
	s  <- as.numeric(readline("\nScale ? :\n"))
	DIST <- rcauchy(Nb,l,s)
} else {
	stop("\n!! Invalid choice, try again !!\n")
}
output <- DIST
}
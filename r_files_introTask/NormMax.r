# Function 'NormMax'
# Killian Martin--Horgassan
# 19-02-2015

# Generates the list of maxima [M_1, ..., M_n] of the list of
# i.i.d random variables [X_1,...,X_n]. Plots the M_i against 
# the i.

# Arguments :
# - N     : number of r.v. X_i
# - DIST  : a vector of N pseudo-random numbers from a certain 
#			distribution.
# RETURNS the list of maxima

NormMax <- function(N = 10000, DIST = rnorm(10000,0,1)) {
 	ListX   <- DIST
 	ListMax <- rep(0,N)
 	
 	# Computes the list of the M_i
 	for (i in 1:length(ListMax)) {
 		ListMax[i] <- max(ListX[1:i])
 	}
 	
 	# Plots the 1-D scatter plot of the M_i
 	title_1 <- "1-D scatter plot of the maxima"
 	xlabel_1 <- expression(M[i])
 	stripchart(ListMax, xlab = xlabel_1, main = title_1)
 	
 	# Opens a new graphic window
 	quartz()
 	
 	# Plots the M_i against the i
 	title_2 <- "Maxima as a function of the time steps"
 	xlabel_2 <- "Time step"
 	ylabel_2 <- "Maximum"
 	plot(ListMax, xlab = xlabel_2, ylab = ylabel_2, main =
 			title_2)
 	
 	# Opens a new graphic windows
 	quartz()
 	
 	# Plot the 1-D scatter plot of the M_i and the M_i against the
 	# i together in a grid.
 	par(mfrow = c(1,2))
 	
 	stripchart(ListMax, xlab = xlabel_1, main = title_1)
    plot(ListMax, xlab = xlabel_2, ylab = ylabel_2, main =
 			title_2, pch = 4)
 	points(1:N,sqrt(log(1:N)),col="cyan", pch = 3)
 	legend(x="bottomright",y=NULL,c("Maxima",
 		expression("sqrt(log(n))")), col = c("black","cyan"),
 		bty="o", pch = c(4,3))
 		
 	# Exports the plots to a PDF file
 	pdf(file = "graphIntroTask.pdf", width = 10, height = 8)
	par(mfrow = c(1,2))
 	stripchart(ListMax, xlab = xlabel_1, main = title_1)
    plot(ListMax, xlab = xlabel_2, ylab = ylabel_2, main =
 			title_2, pch = 4)
 	points(1:N,sqrt(log(1:N)),col="cyan", pch = 3)
 	legend(x="bottomright",y=NULL,c("Maxima",
 		expression("sqrt(log(n))")), col = c("black","cyan"),
 		bty="o", pch = c(4,3))
 	dev.off()
 	
 	return(ListMax)
 }
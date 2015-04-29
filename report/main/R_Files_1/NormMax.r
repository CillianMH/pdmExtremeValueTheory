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
#           (by default it is a Gaussian of size 10000)
# RETURNS the list of maxima

NormMax <- function(N = 10000, DIST = rnorm(10000,0,1), DIST_name = "gaussian") {
 	ListX   <- DIST
 	ListMax <- rep(0,N)
 	
 	print(DIST_name)
 	
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
 	points(1:N,sqrt(2*log(1:N)),col="cyan", pch = 3)
 	legend(x="bottomright",y=NULL,c("Maxima",
 		expression("sqrt(log(n))")), col = c("black","cyan"),
 		bty="o", pch = c(4,3))
 		
 	# Exports the plots to a PDF file
 	pdf(file = "graphIntroTask.pdf", width = 10, height = 8)
	par(mfrow = c(1,2))
 	stripchart(ListMax, xlab = xlabel_1, main = title_1)
    plot(ListMax, xlab = xlabel_2, ylab = ylabel_2, main =
 			title_2, pch = 4)
 	points(1:N,sqrt(2*log(1:N)),col="cyan", pch = 3)
 	legend(x="bottomright",y=NULL,c("Maxima",
 		expression("sqrt(log(n))")), col = c("black","cyan"),
 		bty="o", pch = c(4,3))
 	#dev.off()
 	
 	# Exports the plots to jpeg files for use in the LaTex report
 	
 	if (DIST_name == "gaussian") {
 		fileName1 <- "gaussianScatterMaxima.jpeg"
 		fileName2 <- "gaussianMaximaAgainstSteps.jpeg"
 		fileName3 <- "gaussianFitting.jpeg"
 	} else if (DIST_name == "exponential") {
 		fileName1 <- "exponentialScatterMaxima.jpeg"
 		fileName2 <- "exponentialMaximaAgainstSteps.jpeg" 	
 		fileName3 <- "exponentialFitting.jpeg"
 	} else {
 		fileName1 <- "cauchyScatterMaxima.jpeg"
 		fileName2 <- "cauchyMaximaAgainstSteps.jpeg" 
 		fileName3 <- "cauchyFitting.jpeg"
 	}
 	
 	jpeg(file = fileName1)
 	stripchart(ListMax, xlab = xlabel_1, main = title_1)
 	
 	jpeg(file = fileName2)
 	plot(ListMax, xlab = xlabel_2, ylab = ylabel_2, main =
 			title_2)
 	
 	if (DIST_name == "gaussian") {
 		fileName3 <- "gaussianFitting.jpeg"
 		jpeg(file = fileName3)
        plot(ListMax, xlab = xlabel_2, ylab = ylabel_2, main = 
        title_2, pch = 4)
 	    points(1:N,sqrt(2*log(1:N)),col="cyan", pch = 3)
 	    legend(x="bottomright",y=NULL,c("Maxima",
 	    expression("sqrt(2*log(n))")), col = c("black","cyan"),
 		bty="o", pch = c(4,3))
 	} else if (DIST_name == "exponential") {
 		fileName3 <- "exponentialFitting.jpeg"
 		jpeg(file = fileName3)
        plot(ListMax, xlab = xlabel_2, ylab = ylabel_2, main = 
        title_2, pch = 4)
 	    points(1:N,log(1:N),col="cyan", pch = 3)
 	    legend(x="bottomright",y=NULL,c("Maxima",
 	    expression("log(n)")), col = c("black","cyan"),
 		bty="o", pch = c(4,3))
 	} else {
 		fileName3 <- "cauchyFitting.jpeg"
 		jpeg(file = fileName3)
        plot(ListMax, xlab = xlabel_2, ylab = ylabel_2, main = 
        title_2, pch = 4)
 	    points(1:N,tan(pi*(1:N-2)/(2*1:N)),col="cyan", pch = 3)
 	    legend(x="bottomright",y=NULL,c("Maxima",
 	    expression("tan(pi*(n-2)/2n)")), col = c("black","cyan"),
 		bty="o", pch = c(4,3))
 	}
 			
 	dev.off()
 	

 	
 	return(ListMax)
 }
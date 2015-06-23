# Master Thesis Project - Extreme Value Theory
# We assume that the stock prices follow a geometric
# Brownian motion (Black-Scholes model). We plot the
# empirical stock price data as well as the simulated
# geometric Brownian motions associated to them.
# Killian Martin--Horgassan
# 08-06-2015

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("loadStockData_plain.r")
source("firstElement.r")
source("computeApproximateGradient.r")
source("meanAndVariance.r")
source("Visualisation_StockPriceVariations.r")


# Remark : the csv uses the metastock data format 
#          7 columns : - Ticker (identifier of the stock
#                       and stockmarket on which it is listed)
#					   - Date (yyyymmdd)
#					   - Open
#					   - High
#					   - Low
#					   - Close
#					   - Volume
# By default, the highest price is used i.e. column 3.
CHOICE <- 3

# Loading the original data for the five stocks then
# getting the LVMH and the Total data
df_plain <- loadStockData_plain(CHOICE)
lvmhData <- df_plain[[3]]
totalData <- df_plain[[5]]
indexes <- Visualisation_StockPriceVariations()
#indexes[1] <- indexes[1] - 100

# Building the Geometric Brownian motion 
	N = max(indexes)-1
	M = dim(df)[2]
	simulationLVMH <- rep(0,indexes[1]-1)
	simulationTotal <- rep(0,indexes[2]-1)
	
	# Computing the mean and Variances of the stocks
	meanAndVarLVMH <- meanAndVariance(lvmhData[1:indexes[1]-1])
	meanAndVarTotal <- meanAndVariance(totalData[1:indexes[2]-1])
	
	#######################################################################
	# From experience we see that we need to rescale the time steps if we 
	# want to be able not to go over the max and min values allowed by the
	# computer
	#######################################################################
	
	# Time component
	timeSteps = 0:(N-1)
	timeSteps = timeSteps/N ## RESCALING
	  
	# Brownian motion component
    dis = (1/sqrt(N))*rnorm(N,0,1) ## DO NOT FORGET TO RESCALE HERE
    dis = cumsum(dis)
    

	# LVMH stock
	coeff1 = meanAndVarLVMH[1] - 0.5*meanAndVarLVMH[2]
	coeff1 = coeff1*(1/N) ## Rescale the mean
	coeff2 = sqrt(meanAndVarLVMH[2]) ## The variance has already been rescaled
	cat("****************************************************")
	cat("\nDeterministic coefficient GBM - LVMH simulation:\n")
	print(coeff1)
	cat("\nStochastic coefficient GBM - LVMH simulation:\n")
	print(coeff2)
	exponentLVMH <- rep(0,(indexes[1]-1))
	for (k in 2:(indexes[1] - 1)){
		exponentLVMH[k] <- coeff1*timeSteps[k] + coeff2*dis[k]
	}
	#exponentLVMH[2:(indexes[1]-1)] <- coeff1*timeSteps[2:(indexes[1]-1)] + 
		#coeff2*dis[2:(indexes[1]-1)]
	lvmhT0 <- (df_plain[[3]])[1]
	simulationLVMH <- lvmhT0*exp(exponentLVMH)
	
	# Total Stock
	coeff1 = meanAndVarTotal[1] - 0.5*meanAndVarTotal[2]
	coeff1 = coeff1*(1/N) ## Rescale the mean
	coeff2 = sqrt(meanAndVarTotal[2]) ## The variance has already been rescaled
	cat("\nDeterministic coefficient GBM - Total simulation:\n")
	print(coeff1)
	cat("\nStochastic coefficient GBM - Total simulation:\n")
	print(coeff2)
	cat("****************************************************\n")
	cat("****************************************************\n")
	exponentTotal <- rep(0,(indexes[2]-1))
	for (k in 2:(indexes[2] - 1)){
		exponentTotal[k] <- coeff1*timeSteps[k] + coeff2*dis[k]
	}
	#exponentTotal[2:(indexes[2]-1)] <- coeff1*timeSteps[2:(indexes[2]-1)] +
		#coeff2*dis[2:(indexes[2]-1)]
	totalT0 <- (df_plain[[5]])[1]
	simulationTotal <- totalT0*exp(exponentTotal)
	 
	#######################################################################
	# From experience again we see that we need to clean up a bit the data
	# we have just simulated as there a few points for which the value is
	# extremely high. We will replace them with the closest more 'reasonable'
	# value
	#######################################################################
	# Using the median is a RUBBISH-grade idea
	#simulationLVMH <- replaceExtremes(simulationLVMH,median(simulationLVMH))
	#simulationTotal <- replaceExtremes(simulationTotal,median(simulationTotal))
		 
	 quartz()
     par(mfrow = c(2,4))
     
	 plot(log(lvmhData[1:(indexes[1]-1)]), type = 'l', main = "LVMH - Actual",
	 	ylab = "Log of the Stock price")
	 plot(exponentLVMH + rep(log(lvmhT0),length(exponentLVMH)), type = 'l',
	  main = "LVMH - simulation", ylab = "Log of the Stock price - simulated")
	 plot(exp(exponentLVMH + rep(log(lvmhT0),length(exponentLVMH))), type = 'l',
	  main = "LVMH - simulation", ylab = "Exp of the Log of the Stock price - simulated")
	 plot(simulationLVMH, type = 'l', main = "LVMH - simulation")
	 cat("\n \n \nMax simulation LVMH\n")
	 print(max(simulationLVMH))
	 
	 plot(log(totalData[1:(indexes[2]-1)]), type = 'l', main = "Total - Actual",
	 	ylab = "Log of the Stock price")
	 plot(exponentTotal + rep(log(totalT0),length(exponentTotal)), type = 'l',
	  main = "Total - simulation", ylab = "Log of the Stock price - simulated")
	 plot(exp(exponentTotal + rep(log(totalT0),length(exponentTotal))), type = 'l',
	  main = "Total - simulation", ylab = "Exp of the Log of the Stock price - simulated")
	 plot(simulationTotal, type = 'l', main = "Total - simulation ")
	 cat("\n \n \nMax simulation Total\n")
	 print(max(simulationTotal))


# Plotting the actual stock prices and the corresponding simulations
x_label <- "Weekly measurements"
y_label1 <- "Log Prices : actual and simulation"
y_label2 <- "Prices : actual and simulation"
title1 <- "LVMH Stock"
title2 <- "Total Stock"

# Rescaling the y-axis by computing the maxima and minima
# and adapting the y-min and y-max
Ymaxima <- c(max((df_plain[[3]])[1:indexes[1]-1],simulationLVMH),
				max((df_plain[[5]])[1:indexes[2]-1],simulationTotal))
Yminima <- c(min((df_plain[[3]])[1:indexes[1]-1],simulationLVMH),
				min((df_plain[[5]])[1:indexes[2]-1],simulationTotal))
				
quartz()
png(file = "cleanedLVMH_Total_Log.png")
par(mfrow = c(1,2))
plot(log(lvmhData[1:(indexes[1]-1)]), pch = 1, col = "green", type = 'l', xlab = x_label, ylab = y_label1, main = title1, ylim = c(0,10))
lines(exponentLVMH + rep(log(lvmhT0),length(exponentLVMH)), col = "yellow")
print(length(exponentLVMH))
print(length(log(lvmhData[1:(indexes[1]-1)])))
plot(log(totalData[1:(indexes[2]-1)]), pch = 1, col = "red", type = 'l', xlab = x_label, ylab = y_label1, main = title2, ylim = c(0,10))
lines(exponentTotal + rep(log(totalT0),length(exponentTotal)), col = "yellow")
#dev.off()
#graphics.off()


quartz()
png(file = "cleanedLVMH_Total.png")
par(mfrow = c(1,2))
plot(exp(log((df_plain[[3]])[1:(indexes[1]-1)])), pch = 1, col = "green", type = 'l', xlab = x_label, ylab = y_label2, main = title1, ylim = c(Yminima[1],1000))
lines(exp(exponentLVMH + rep(log(lvmhT0),length(exponentLVMH))), col = "yellow")
plot(exp(log((df_plain[[5]])[1:(indexes[2]-1)])), pch = 1, col = "red", type = 'l', xlab = x_label, ylab = y_label2, main = title2, ylim = c(Yminima[2],1000))
lines(exp(exponentTotal + rep(log(totalT0),length(exponentTotal))), col = "yellow")

	






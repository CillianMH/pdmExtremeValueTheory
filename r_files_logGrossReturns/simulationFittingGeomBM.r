# Master Thesis Project - Extreme Value Theory
# We assume that the stock prices follow a geometric
# Brownian motion (Black-Scholes model). We plot the
# empirical stock price data as well as the simulated
# geometric Brownian motions associated to them.
# Killian Martin--Horgassan
# 02-06-2015

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("logGrossReturns.r")
source("loadStockData.r")
source("loadStockData_plain.r")
source("meanAndVarianceDF.r")


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

# Loading the original data for the five stocks
df_plain <- loadStockData_plain(CHOICE)

# Loading the log-gross returns for the five stocks
df <- loadStockData(CHOICE)

# Computing the mean and Variances of the stock
meanAndVarDF <- meanAndVarianceDF(df)

# Building the Geometric Brownian motion
	N = dim(df)[1]
	M = dim(df)[2]
	c <- rep(0,N)
	geomBMs <- data.frame(c,c,c,c,c)
	
	means <- meanAndVarDF[1]
	vars <- meanAndVarDF[2]
	
	# Time component
	timeSteps = 0:(N-1)
	  
	# Brownian motion component
    dis = rnorm(N,0,1)
    dis = cumsum(dis)
    

	for (i in 1:M) {
		coeff1 = (means[[1]])[i] - 0.5*(vars[[1]])[i]
		geomBMs[i] <- (df_plain[[i]])[1]*exp(coeff1*timeSteps + sqrt((vars[[1]])[i])*dis)
	}



# Plotting the actual stock prices and the corresponding simulations
x_label <- "Weekly measurements"
y_label <- "Prices and simulation"
title1 <- "BNP Stock"
title2 <- "Carrefour Stock"
title3 <- "LVMH Stock"
title4 <- "Sanofi Stock"
title5 <- "Total Stock"

quartz()
png(file = "actualAndSimulated.png")
par(mfrow = c(3,2))
plot(df_plain[[1]], pch = 1, col = "black", type = 'l', xlab = x_label, ylab = y_label, main = title1)
lines(geomBMs[[1]], col = 'yellow')
plot(df_plain[[2]], pch = 1, col = "blue", type = 'l', xlab = x_label, ylab = y_label, main = title2)
lines(geomBMs[[2]], col = "yellow")
plot(df_plain[[3]], pch = 1, col = "green", type = 'l', xlab = x_label, ylab = y_label, main = title3)
lines(geomBMs[[3]], col = "yellow")
plot(df_plain[[4]], pch = 1, col = "purple", type = 'l', xlab = x_label, ylab = y_label, main = title4)
lines(geomBMs[[4]], col = "yellow")
plot(df_plain[[5]], pch = 1, col = "red", type = 'l', xlab = x_label, ylab = y_label, main = title5)
lines(geomBMs[[5]], col = "yellow")
dev.off()
graphics.off()

# Rescaling the y-axis by computing the maxima and minima
# and adapting the y-min and y-max
Ymaxima <- c(max(df_plain[[1]],geomBMs[[1]]), max(df_plain[[2]],geomBMs[[2]]),
	max(df_plain[[3]],geomBMs[[3]]), max(df_plain[[4]],geomBMs[[4]]),
	max(df_plain[[5]],geomBMs[[5]]))
Yminima <- c(min(df_plain[[1]],geomBMs[[1]]), min(df_plain[[2]],geomBMs[[2]]),
	min(df_plain[[3]],geomBMs[[3]]), min(df_plain[[4]],geomBMs[[4]]),
	min(df_plain[[5]],geomBMs[[5]]))
	
# Plotting with the y-axes properly scaled
quartz()
png(file = "actualAndSimulatedRescaled.png")
par(mfrow = c(3,2))
plot(df_plain[[1]], pch = 1, col = "black", type = 'l', xlab = x_label, ylab = y_label, main = title1, ylim = c(Yminima[1], Ymaxima[1]))
lines(geomBMs[[1]], col = 'yellow')
plot(df_plain[[2]], pch = 1, col = "blue", type = 'l', xlab = x_label, ylab = y_label, main = title2, ylim = c(Yminima[2], Ymaxima[2]))
lines(geomBMs[[2]], col = "yellow")
plot(df_plain[[3]], pch = 1, col = "green", type = 'l', xlab = x_label, ylab = y_label, main = title3, ylim = c(Yminima[3], Ymaxima[3]))
lines(geomBMs[[3]], col = "yellow")
plot(df_plain[[4]], pch = 1, col = "purple", type = 'l', xlab = x_label, ylab = y_label, main = title4, ylim = c(Yminima[4], Ymaxima[4]))
lines(geomBMs[[4]], col = "yellow")
plot(df_plain[[5]], pch = 1, col = "red", type = 'l', xlab = x_label, ylab = y_label, main = title5,
ylim = c(Yminima[5], Ymaxima[5]))
lines(geomBMs[[5]], col = "yellow")
dev.off()
graphics.off()






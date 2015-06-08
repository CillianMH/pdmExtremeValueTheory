# Master Thesis Project - Extreme Value Theory
# We assume that the stock prices follow a geometric
# Brownian motion (Black-Scholes model). We plot the
# empirical stock price data as well as the simulated
# geometric Brownian motions associated to them.
# HERE WE WILL RUN A LARGE NUMBER OF SIMULATIONS
# AND TAKE THE AVERAGE (for instance 100 simulations)
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

# Number of simulations
nb_sim <- 100

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

for (j in 1:nb_sim)	 {
	# Brownian motion component
    dis = rnorm(N,0,1)
    dis = cumsum(dis)
    
	for (i in 1:M) {
		coeff1 = (means[[1]])[i] - 0.5*(vars[[1]])[i]
		geomBMs[i] <- geomBMs[i] + (df_plain[[i]])[1]*exp(coeff1*timeSteps + sqrt((vars[[1]])[i])*dis)
	}	
} 

for (i in 1:M) {
	geomBMs[i] <- geomBMs[i]/nb_sim 
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
png(file = "actualAndSimulated-Averaged.png")
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





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

# Loading the original data for the LVMHH and Total
# stocks and getting the indexes where to cut the 
# data.
df_plain <- loadStockData_plain(CHOICE)
indexes <- Visualisation_StockPriceVariations()
index_lvmh <- indexes[1]-1
index_total <- indexes[2]-1

S_lvmh <- df_plain[[3]]
S_total <- df_plain[[5]]

S_lvmh <- S_lvmh[1:index_lvmh]
S_total <- S_total[1:index_total]

LS_lvmh <- log(S_lvmh)
LS_total <- log(S_total)

LR_lvmh <- diff(LS_lvmh)
LR_total <- diff(LS_total)

index_lvmh <- length(LR_lvmh)
index_total <- length(LR_total)

m_lvmh <- mean(LR_lvmh)
s2_lvmh <- var(LR_lvmh)

m_total <- mean(LR_total)
s2_total <- var(LR_total)

# Building the Geometric Brownian motions 
B_lvmh1 <- rnorm(index_lvmh,0,1)
B_lvmh1 <- S_lvmh[1]*exp(cumsum((m_lvmh + 0.5*s2_lvmh) + sqrt(s2_lvmh)*B_lvmh1))

B_lvmh2 <- rnorm(index_lvmh,0,1)
B_lvmh2 <-  S_lvmh[1]*exp(cumsum((m_lvmh + 0.5*s2_lvmh) + sqrt(s2_lvmh)*B_lvmh2))

B_lvmh3 <- rnorm(index_lvmh,0,1)
B_lvmh3 <-  S_lvmh[1]*exp(cumsum((m_lvmh + 0.5*s2_lvmh) + sqrt(s2_lvmh)*B_lvmh3))

B_lvmh4 <- rnorm(index_lvmh,0,1)
B_lvmh4 <-  S_lvmh[1]*exp(cumsum((m_lvmh + 0.5*s2_lvmh) + sqrt(s2_lvmh)*B_lvmh4))

B_lvmh5 <- rnorm(index_lvmh,0,1)
B_lvmh5 <-  S_lvmh[1]*exp(cumsum((m_lvmh + 0.5*s2_lvmh) + sqrt(s2_lvmh)*B_lvmh5))

minima_lvmh <- min(c(B_lvmh1, B_lvmh2, B_lvmh3, B_lvmh4, B_lvmh5))
maxima_lvmh <- max(c(B_lvmh1, B_lvmh2, B_lvmh3, B_lvmh4, B_lvmh5))

B_total1 <- rnorm(index_total,0,1)
B_total1 <- S_total[1]*exp(cumsum((m_total + 0.5*s2_total) + sqrt(s2_total)*B_total1))

B_total2 <- rnorm(index_total,0,1)
B_total2 <- S_total[1]*exp(cumsum((m_total + 0.5*s2_total) + sqrt(s2_total)*B_total2))

B_total3 <- rnorm(index_total,0,1)
B_total3 <- S_total[1]*exp(cumsum((m_total + 0.5*s2_total) + sqrt(s2_total)*B_total3))

B_total4 <- rnorm(index_total,0,1)
B_total4 <- S_total[1]*exp(cumsum((m_total + 0.5*s2_total) + sqrt(s2_total)*B_total4))

B_total5 <- rnorm(index_total,0,1)
B_total5 <- S_total[1]*exp(cumsum((m_total + 0.5*s2_total) + sqrt(s2_total)*B_total5))

minima_total <- min(c(B_total1, B_total2, B_total3, B_total4, B_total5))
maxima_total <- max(c(B_total1, B_total2, B_total3, B_total4, B_total5))

quartz()
dev.new(width = 7, heigth = 7)
png(file = "cleanedLVMH.png")
plot(1:index_lvmh, S_lvmh[1:index_lvmh], col = 'grey', lwd = 2,
	type = 'l', main = 'LVMH Stock actual & simulated', xlab =
    'Time steps in weeks', ylab = 'Stock prices', ylim = 
    c(minima_lvmh, maxima_lvmh))
lines(1:index_lvmh, B_lvmh1, col = 'red')
lines(1:index_lvmh, B_lvmh2, col = 'blue')
lines(1:index_lvmh, B_lvmh3, col = 'green')
lines(1:index_lvmh, B_lvmh4, col = 'yellow')
lines(1:index_lvmh, B_lvmh5, col = 'purple')
dev.off()
graphics.off()

quartz()
dev.new(width = 7, heigth = 7)
png(file = "cleanedTotal.png")
plot(1:index_total, S_total[1:index_total], col = 'grey', lwd = 2,
	type = 'l', main = 'Total Stock actual & simulated', xlab =
    'Time steps in weeks', ylab = 'Stock prices', ylim = 
    c(minima_total, maxima_total))
lines(1:index_total, B_total1, col = 'red')
lines(1:index_total, B_total2, col = 'blue')
lines(1:index_total, B_total3, col = 'green')
lines(1:index_total, B_total4, col = 'yellow')
lines(1:index_total, B_total5, col = 'purple')
dev.off()
graphics.off()

B_totalBeta1 <- rnorm(6*index_total,0,1/sqrt(6))
B_totalBeta1 <- S_total[1]*exp(cumsum((m_total + 0.5*s2_total) + sqrt(s2_total)*B_totalBeta1))

B_totalBeta2 <- rnorm(6*index_total,0,1/sqrt(6))
B_totalBeta2 <- S_total[1]*exp(cumsum((m_total + 0.5*s2_total) + sqrt(s2_total)*B_totalBeta2))

minima_total_Beta <- min(c(B_totalBeta1, B_totalBeta2))
maxima_total_Beta <- max(c(B_totalBeta1, B_totalBeta2))


quartz()
dev.new(width = 7, heigth = 7)
png(file = "cleanedTotal_better_simulation.png")
plot(1:index_total, S_total[1:index_total], col = 'grey', lwd = 2,
	type = 'l', main = 'Total Stock actual [weekly increment] & \n simulated [daily increment]', xlab =
    'Time steps in weeks', ylab = 'Stock prices')#, ylim = 
    #c(minima_total_Beta, maxima_total_Beta))
lines(1:(6*index_total), B_totalBeta1, col = 'purple')
lines(1:(6*index_total), B_totalBeta2, col = 'blue')

dev.off()
graphics.off()


B_lvmhBeta1 <- rnorm(6*index_lvmh,0,1/sqrt(6))
B_lvmhBeta1 <- S_lvmh[1]*exp(cumsum((m_lvmh + 0.5*s2_lvmh) + sqrt(s2_lvmh)*B_lvmhBeta1))

B_lvmhBeta2 <- rnorm(6*index_lvmh,0,1/sqrt(6))
B_lvmhBeta2 <- S_lvmh[1]*exp(cumsum((m_lvmh + 0.5*s2_lvmh) + sqrt(s2_lvmh)*B_lvmhBeta2))

minima_lvmh_Beta <- min(c(B_lvmhBeta1, B_lvmhBeta2))
maxima_lvmh_Beta <- max(c(B_lvmhBeta1, B_lvmhBeta2))

quartz()
dev.new(width = 7, heigth = 7)
png(file = "cleanedLVMH_better_simulation.png")
plot(1:index_lvmh, S_lvmh[1:index_lvmh], col = 'grey', lwd = 2,
	type = 'l', main = 'LVMH Stock actual [weekly increment] & \n simulated [daily increment]', xlab =
    'Time steps in weeks', ylab = 'Stock prices')#, ylim = 
    #c(minima_total_Beta, maxima_total_Beta))
lines(1:(6*index_lvmh), B_lvmhBeta1, col = 'purple')
lines(1:(6*index_lvmh), B_lvmhBeta2, col = 'blue')

dev.off()
graphics.off()







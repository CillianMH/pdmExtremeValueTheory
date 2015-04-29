# Some plottings : prices, returns and log of gross returns
# 17-03-2015
# Killian Martin--Horgassan

# Clear the environment
rm(list=ls())

# Close all already open graphic windows 
graphics.off()

N <- 100
Xt <- rep(0,N)
Rt <- rep(0,N-1)
r_t <- rep(0,N-1)

# A brownian motion, used to model the evolution of the price 
# of a stock
for (i in 1:N) {
	Xt[i] <- rnorm(1, mean = N, sd = i)
}

# The returns and log of gross returns
for (i in 1:N-1) {
	Rt[i] <- log(Xt[i+1]/Xt[i])
	r_t[i] <- (Xt[i+1]-Xt[i])/Xt[i]
}

quartz()
par(mfrow = c(1,3))
plot(Xt, xlab = 'Time step', ylab = 'Stock Price',type = 'l', col = 'black')
plot(r_t, xlab = 'Time step', ylab = 'Returns', type = 'l', col = 'red')
plot(Rt, xlab = 'Time step', ylab = 'Log of gross returns', type = 'l', col = 'blue')

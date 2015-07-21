# Master Thesis Project - Extreme Value Theory
# For each sample, ordering the data and comparing
# the ordered sample to the quantiles of the 
# extreme law fitting the tail of the original
# sample

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("loadStockData_plain.r")

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

# Computing the 95 % - quantiles for each of the stocks
quantiles <- c(quantile(df_plain[[1]],0.95), quantile(df_plain[[2]],0.95), quantile(df_plain[[3]],0.975),
					quantile(df_plain[[4]],0.95), quantile(df_plain[[5]],0.95))
					
# Getting the data above the threshold
data_bnp <- df_plain[[1]]
data_bnp <- data_bnp[data_bnp > quantiles[1]]

data_carrefour <- df_plain[[2]]
data_carrefour <- data_carrefour[data_carrefour > quantiles[2]]

data_lvmh <- df_plain[[3]]
data_lvmh <- data_lvmh[data_lvmh > quantiles[3]]

data_sanofi <- df_plain[[4]]
data_sanofi <- data_sanofi[data_sanofi > quantiles[4]]

data_total <- df_plain[[5]]
data_total <- data_total[data_total > quantiles[5]]

str(gev.fit(data_total))
# Fitting the data, and getting the corresponding location, scale and 
# shape parameters respectively, for each stock
bnp_fit <- (gev.fit(data_bnp))
carrefour_fit <- gev.fit(data_carrefour)
lvmh_fit <- gev.fit(data_lvmh)
sanofi_fit <- gev.fit(data_sanofi)
total_fit <- gev.fit(data_total)

params_bnp <- bnp_fit$mle
params_carrefour <- carrefour_fit$mle
params_lvmh <- lvmh_fit$mle
params_sanofi <- sanofi_fit$mle
params_total <- total_fit$mle

# Ordering the samples (increasing sort)
data_bnp <- sort(df_plain[[1]])
data_carrefour <- sort(df_plain[[2]])
data_lvmh <- sort(df_plain[[3]])
data_sanofi <- sort(df_plain[[4]])
data_total <- sort(df_plain[[5]])

# Vector of the quantiles we want to plot
q <- seq(0.99/800,0.99,0.99/800)

plot(data_bnp, col = 'grey', type = 'l')
gev_bnp <- qgev(q,params_bnp[3],params_bnp[1],params_bnp[2])
lines(gev_bnp, col = 'red')
print(bnp_fit$mle)
print(carrefour_fit$mle)
print(lvmh_fit$mle)
print(sanofi_fit$mle)
print(total_fit$mle)

quartz()
png(file = "DataVSQuantiles_BNP.png")
plot(data_bnp, col = 'green', type = 'l', main = "Sorted BNP Paribas stock prices and quantiles of the extreme law")
gev_bnp <- qgev(q,params_bnp[3],params_bnp[1],params_bnp[2])
lines(gev_bnp, col = 'black')
dev.off()
graphics.off()

quartz()
png(file = "DataVSQuantiles_Carrefour.png")
plot(data_carrefour, col = 'red', type = 'l', main = "Sorted Carrefour stock prices and quantiles of the extreme law")
gev_carrefour <- qgev(q,params_carrefour[3],params_carrefour[1],params_carrefour[2])
lines(gev_carrefour, col = 'black')
dev.off()
graphics.off()

quartz()
png(file = "DataVSQuantiles_LVMH.png")
plot(data_lvmh, col = 'yellow', type = 'l', main = "Sorted LVMH stock prices and quantiles of the extreme law")
gev_lvmh <- qgev(q,params_lvmh[3],params_lvmh[1],params_lvmh[2])
lines(gev_lvmh, col = 'black')
dev.off()
graphics.off()

quartz()
png(file = "DataVSQuantiles_Sanofi.png")
plot(data_sanofi, col = 'grey', type = 'l', main = "Sorted Sanofi stock prices and quantiles of the extreme law")
gev_sanofi <- qgev(q,params_sanofi[3],params_sanofi[1],params_sanofi[2])
lines(gev_sanofi, col = 'black')
dev.off()
graphics.off()

quartz()
png(file = "DataVSQuantiles_Total.png")
plot(data_total, col = 'blue', type = 'l', main = "Sorted Total stock prices and quantiles of the extreme law")
gev_total <- qgev(q,params_total[3],params_total[1],params_total[2])
lines(gev_total, col = 'black')
dev.off()
graphics.off()






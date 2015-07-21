# Master Thesis Project - Extreme Value Theory
# Getting the experimental data above the 95 %
# and fitting a GEV distribution to it

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
quantiles <- c(quantile(df_plain[[1]],0.95), quantile(df_plain[[2]],0.95), quantile(df_plain[[3]],0.95),
					quantile(df_plain[[4]],0.95), quantile(df_plain[[5]],0.95))
					
# Getting the data above the threshold

data_bnp <- df_plain[[1]]
data_bnp <- data_bnp[data_bnp > quantiles[1]]
data_bnp <- diff(log(data_bnp))

data_carrefour <- df_plain[[2]]
data_carrefour <- data_carrefour[data_carrefour > quantiles[2]]
data_carrefour <- diff(log(data_carrefour))

data_lvmh <- df_plain[[3]]
data_lvmh <- data_lvmh[data_lvmh > quantiles[3]]
data_lvmh <- diff(log(data_lvmh))

data_sanofi <- df_plain[[4]]
data_sanofi <- data_sanofi[data_sanofi > quantiles[4]]
data_sanofi <- diff(log(data_sanofi))

data_total <- df_plain[[5]]
data_total <- data_total[data_total > quantiles[5]]
data_total <- diff(log(data_total))

# Plotting the above-threshold data
x_label <- ""
y_label <- "Stock price"
title1 <- "BNP Paribas log-returns"
title2 <- "Carrefour log-returns"
title3 <- "LVMH  log-returns"
title4 <- "Sanofi  log-returns"
title5 <- "Total  log-returns"
quartz()
png(file = "aboveThresholdData.png")
par(mfrow = c(3,2))
plot(data_bnp, pch = 1, col = "black", type = 'p', xlab = x_label, ylab = y_label, main = title1)
plot(data_carrefour, pch = 1, col = "blue", type = 'p', xlab = x_label, ylab = y_label, main = title2)
plot(data_lvmh, pch = 1, col = "green", type = 'p', xlab = x_label, ylab = y_label, main = title3)
plot(data_sanofi, pch = 1, col = "purple", type = 'p', xlab = x_label, ylab = y_label, main = title4)
plot(data_total, pch = 1, col = "red", type = 'p', xlab = x_label, ylab = y_label, main = title5)
dev.off()
graphics.off()		
			
quartz()
png(file = "gev.diag BNP Paribas")
gev.diag(gev.fit(data_bnp))
dev.off()
graphics.off()

quartz()
png(file = "gev.diag Carrefour")
gev.diag(gev.fit(data_carrefour))
dev.off()
graphics.off()

quartz()
png(file = "gev.diag LVMH")
gev.diag(gev.fit(data_lvmh))
dev.off()
graphics.off()

quartz()
png(file = "gev.diag Sanofi")
gev.diag(gev.fit(data_sanofi))
dev.off()
graphics.off()

quartz()
png(file = "gev.diag Total")
gev.diag(gev.fit(data_total))
dev.off()
graphics.off()

data_lvmh <- df_plain[[3]]
data_lvmh <- data_lvmh[data_lvmh > quantile(df_plain[[3]],0.975)]
data_lvmh <- diff(log(data_lvmh))

quartz()
png(file = "gev.diag LVMH2")
gev.diag(gev.fit(data_lvmh))
dev.off()
graphics.off()

data_lvmh <- as.numeric((read.csv("daily_LVMH_table.csv", header = FALSE))[[3]])
quartz()
png(file = "LVMH_daily.png")
plot(1:length(data_lvmh), data_lvmh)
dev.off()
graphics.off()

lvmh_quant <- quantile(data_lvmh, 0.99)
data_lvmh <- data_lvmh[data_lvmh > lvmh_quant]
data_lvmh <- diff(log(data_lvmh))


quartz()
png(file = "LVMH_daily_99quant.png")
plot(1:length(data_lvmh), data_lvmh)
dev.off()
graphics.off()

quartz()
png(file = "gev.diag LVMH3")
gev.diag(gev.fit(data_lvmh))
dev.off()
graphics.off()	






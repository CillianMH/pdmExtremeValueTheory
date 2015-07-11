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

data_carrefour <- df_plain[[2]]
data_carrefour <- data_carrefour[data_carrefour > quantiles[2]]

data_lvmh <- df_plain[[3]]
data_lvmh <- data_lvmh[data_lvmh > quantiles[3]]

data_sanofi <- df_plain[[4]]
data_sanofi <- data_sanofi[data_sanofi > quantiles[4]]

data_total <- df_plain[[5]]
data_total <- data_total[data_total > quantiles[5]]


# Plotting the above-threshold data
x_label <- ""
y_label <- "Stock price"
title1 <- "BNP Paribas stock"
title2 <- "Carrefour stock"
title3 <- "LVMH stock"
title4 <- "Sanofi stock"
title5 <- "Total stock"
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
	






	






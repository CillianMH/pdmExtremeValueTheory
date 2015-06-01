# Master Thesis Project - Extreme Value Theory
# Computation of the log-returns + the empirical mean 
# and variances, for the five stocks chosen from CAC 40
# [BNP, Carrefour, LVMH, Sanofi, Total]
# Killian Martin--Horgassan
# 01-06-2015

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("logGrossReturns.r")
source("meanAndVariance.r")
source("loadStockData.r")
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

# Loading the log-gross returns for the five stocks
df <- loadStockData(CHOICE)

# Computing the mean and Variances of the stock
meanAndVarDF <- meanAndVarianceDF(df)


# Plotting the log returns
x_label <- "Weekly measurements"
y_label <- "Log gross returns on the stock"
title1 <- "BNP Stock"
title2 <- "Carrefour Stock"
title3 <- "LVMH Stock"
title4 <- "Sanofi Stock"
title5 <- "Total Stock"
color <- c("black", "blue", "green", "purple", "red")

quartz()
jpeg(file = "logGrossReturns.jpeg", quality = 150)
par(mfrow = c(3,2))
plot(df$V1, pch = 1, col = "black", type = 'l', xlab = x_label, ylab = y_label, main = title1)
plot(df$V2, pch = 1, col = "blue", type = 'l', xlab = x_label, ylab = y_label, main = title2)
plot(df$V3, pch = 1, col = "green", type = 'l', xlab = x_label, ylab = y_label, main = title3)
plot(df$V4, pch = 1, col = "purple", type = 'l', xlab = x_label, ylab = y_label, main = title4)
plot(df$V5, pch = 1, col = "red", type = 'l', xlab = x_label, ylab = y_label, main = title5)
gplots:::textplot(meanAndVarDF, halign="center", valign="center", col.rownames = color)
dev.off()




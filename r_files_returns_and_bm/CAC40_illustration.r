# Master Thesis Project - Extreme Value Theory
# Illustrating the sequence of maxima with a real
# world example : the evolution of the prices of 
# shares of 5 companies listed on CAC40.
# Killian Martin--Horgassan
# 19-02-2015

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("/Users/kimartin/Desktop/R_files_pdm/Tache3/netReturns.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache3/logGrossReturns.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache3/inputCac40_illustration.r")

# Loading the data into variables
STOCK <- InputCAC40_illustration()
Stock <- read.csv(STOCK, header = FALSE)

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

# Getting the right column of the stock dataframe, as well as the
# log gross returns and the net returns.
Stock_processed <- rev(Stock[[CHOICE]])
returnsStock <- netReturns(Stock_processed)
logReturnsStock <- logGrossReturns(Stock_processed)


# Plotting the stock prices, the returns and the log returns

x_label <- "Weekly measurements"
y_label1 <- "Stock price"
y_label2 <- "Net returns on the stock"
y_label3 <- "Log gross returns on the stock"
y_label4 <- "Gross returns on the stock"
y_label5 <- "1 plus the log gross returns on the stock"
title1 <- STOCK
title2 <- "exp(Rt)"
title3 <- "1 + Rt"


# Stock prices - net returns - log gross returns
quartz()
par(mfrow = c(1,3))
plot(Stock_processed, pch = 1, col = "black", type = 'l', xlab = x_label, ylab = y_label1, main = title1)
plot(returnsStock, pch = 1, col = "red", type = 'l', xlab = x_label, ylab = y_label2, main = title1)
plot(logReturnsStock, pch = 1, col = "green", type = 'l', xlab = x_label, ylab = y_label3, main = title1)

# Comparing exp(Rt) and 1+Rt, is the approximation valid in practice
quartz()
par(mfrow = c(1,2))
plot(exp(logReturnsStock), pch = 1, col = "blue", type = 'l', xlab = x_label, ylab = y_label4, main = title2)
plot(1+returnsStock, pch = 1, col = "magenta", type = 'l', xlab = x_label, ylab = y_label5, main = title3)



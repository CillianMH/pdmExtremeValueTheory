# Master Thesis Project - Extreme Value Theory
# A small script to visualise the variations in the
# prices of the stock, using an approximate gradient
# Killian Martin--Horgassan
# 08-06-2015

Visualisation_StockPriceVariations <- function() {
# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("computeApproximateGradient.r")
source("loadStockData_plain.r")
source("firstElement.r")


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

# Building the dataframe of the corresponding gradients
dims <- dim(df_plain)

mat = matrix(0, nrow = dims[1], ncol = dims[2])
gradients = as.data.frame(mat)

	# Storing the maxima and the minima of the gradients
	maxGrad <- rep(0,dims[2])
	minGrad <- rep(0,dims[2])
	
	# Storing the medians of the gradients
	medians <- rep(0,dims[2])

for (i in 1:dims[2]) {
	gradients[[i]] <- computeApproximateGradient(df_plain[[i]])
	maxGrad[i] <- max(gradients[[i]])
	minGrad[i] <- min(gradients[[i]])
	medians[i] <- median(gradients[[i]])
}

# Plotting the gradients
x_label <- "Weekly measurements"
y_label <- "Gradients"
title1 <- "BNP Stock"
title2 <- "Carrefour Stock"
title3 <- "LVMH Stock"
title4 <- "Sanofi Stock"
title5 <- "Total Stock"
quartz()
png(file = "approximatesGradient.png")
par(mfrow = c(3,2))
plot(gradients[[1]], xlab = x_label, ylab = y_label, main = title1)
plot(gradients[[2]], xlab = x_label, ylab = y_label, main = title2)
plot(gradients[[3]], xlab = x_label, ylab = y_label, main = title3)
plot(gradients[[4]], xlab = x_label, ylab = y_label, main = title4)
plot(gradients[[5]], xlab = x_label, ylab = y_label, main = title5)
dev.off()
graphics.off()

cat("The maxima of the gradients for the 5 stocks are :\n")
print(maxGrad)
cat("\nThe minima of the gradients for the 5 stocks are :\n")
print(minGrad)
cat("\nThe medians of the gradients for the 5 stocks are :\n")
print(medians)

# Given the structure of the data, a good threshold to consider is the median
# [The negative gradients are not important as we are going to cut the series
# after the first wide variation of the gradient, which will be positive : the
# price going up by a large amount. Potential subsequent negative gradients 
# thus matter not], or is it ? 

# Actually we will have to rely on the human eye to set a threshold.

# It appears that setting a threshold of 1 is satisfactory for the LVMH and
# Total stocks

LVMHStock <- gradients[[3]] < 1
TotalStock <- gradients[[5]] < 1

cat("\n \n****************************************************\n")
cat("LVMH stock, stop at index:\n")
indexLVMH <- firstElement(LVMHStock)
print(indexLVMH)
cat("****************************************************")
cat("\nTotal stock, stop at index:\n")
indexTotal <- firstElement(TotalStock)
print(indexTotal)
cat("****************************************************\n")

# It appears from running the script that we should stop at week 455
# for the Total stock and at week 762 for the LVMH stock.

output <- c(indexLVMH,indexTotal)
}




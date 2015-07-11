# Function 'loadStockData_plain'
# Killian Martin--Horgassan
# 10-03-2015


# Load the data for the five stocks into a data frame


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

loadStockData_plain <- function(CHOICE = 3) {
	
	STOCK1 <- read.csv("WeeklyBNP.csv", header = FALSE)
	STOCK2 <- read.csv("WeeklyCarrefour.csv", header = FALSE)
	STOCK3 <- read.csv("WeeklyLVMH.csv", header = FALSE)
	STOCK4 <- read.csv("WeeklySanofi.csv", header = FALSE)
	STOCK5 <- read.csv("WeeklyTotal.csv", header = FALSE)
	
	V1 <- rev(STOCK1[[CHOICE]])
	V2 <- rev(STOCK2[[CHOICE]])
	V3 <- rev(STOCK3[[CHOICE]])
	V4 <- rev(STOCK4[[CHOICE]])
	V5 <- rev(STOCK5[[CHOICE]])
		
	# Building the data frame
	df <- data.frame(V1, V2, V3, V4, V5)
	
	output <- df
}
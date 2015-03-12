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
source("/Users/kimartin/Desktop/R_files_pdm/Tache2/InputCAC40_illustration.r")
source("/Users/kimartin/Desktop/R_files_pdm/Tache2/ExtractMaxima.r")

# Loading the data into variables
BNP <- read.csv("BNP_Paribas.csv", header = FALSE)
Carrefour <- read.csv("Carrefour.csv", header = FALSE)
LVMH <- read.csv("LVMH.csv", header = FALSE)
Sanofi <- read.csv("Sanofi.csv", header = FALSE)
Total <- read.csv("Total.csv", header = FALSE)

# Remark : the csv uses the metastock data format 
#          7 columns : - Ticker (identifier of the stock
#                       and stockmarket on which it is listed)
#					   - Date (yyyymmdd)
#					   - Open
#					   - High
#					   - Low
#					   - Close
#					   - Volume

# The user may choose which option between Open -> Close
CHOICE <- as.numeric(InputCAC40_illustration())

# Building the vectors of successive maxima 
# rev is used as the original data is newest -> oldest
BNP_max <- ExtractMaxima(rev(BNP[[CHOICE]]))
Carrefour_max <- ExtractMaxima(rev(Carrefour[[CHOICE]]))
LVMH_max <- ExtractMaxima(rev(LVMH[[CHOICE]]))
Sanofi_max <- ExtractMaxima(rev(Sanofi[[CHOICE]]))
Total_max <- ExtractMaxima(rev(Total[[CHOICE]]))

# Plotting the successive maxima
plot(BNP_max, pch = 1)
#points(1:length(Carrefour_max), Carrefour_max, col="red", pch = 1)
#points(1:length(LVMH_max), LVMH_max, col="cyan", pch = 1)


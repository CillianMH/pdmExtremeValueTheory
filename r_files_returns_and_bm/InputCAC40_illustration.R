# Function 'InputCAC40_illustration'
# Killian Martin--Horgassan
# 10-03-2015

# Manages the input for choice of the data used for the plottings

InputCAC40_illustration <- function() {
cat("Choose the stock to use :\n",
    "- [1] BNP Paribas\n",
	"- [2] Carrefour\n",
	"- [3] LVMH\n",
	"- [4] Sanofi\n",
	"- [5] Total")
choice_dist <- as.character(readline("Your choice ? :\n"))
if (choice_dist == "1") {
	STOCK <- "WeeklyBNP.csv"
} else if (choice_dist == "2") {
	STOCK <- "WeeklyCarrefour.csv"
} else if (choice_dist == "3") {
	STOCK <- "WeeklyLVMH.csv"
} else if (choice_dist == "4") {
	STOCK <- "WeeklySanofi.csv"
} else if (choice_dist == "5") {
	STOCK <- "WeeklyTotal.csv"
} else {
	stop("\n!! Invalid choice, try again !!\n")
}
output <- STOCK
}
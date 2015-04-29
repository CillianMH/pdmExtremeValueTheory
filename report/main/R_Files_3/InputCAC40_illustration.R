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
	ToSave1 <- "WeeklyBNP.jpeg"
	ToSave2 <- "WeeklyBNP_1+Rt_eRt.jpeg"
} else if (choice_dist == "2") {
	STOCK <- "WeeklyCarrefour.csv"
	ToSave1 <- "WeeklyCarrefour.jpeg"
	ToSave2 <- "WeeklyCarrefour_1+Rt_eRt.jpeg"
} else if (choice_dist == "3") {
	STOCK <- "WeeklyLVMH.csv"
	ToSave1 <- "WeeklyLVMH.jpeg"
	ToSave2 <- "WeeklyLVMH_1+Rt_eRt.jpeg"
} else if (choice_dist == "4") {
	STOCK <- "WeeklySanofi.csv"
	ToSave1 <- "WeeklySanofi.jpeg"
	ToSave2 <- "WeeklySanofi_1+Rt_eRt.jpeg"
} else if (choice_dist == "5") {
	STOCK <- "WeeklyTotal.csv"
	ToSave1 <- "WeeklyTotal.jpeg"
	ToSave2 <- "WeeklyTotal_1+Rt_eRt.jpeg"
} else {
	stop("\n!! Invalid choice, try again !!\n")
}
print(list(STOCK,ToSave1,ToSave2))
print("fuck")
output <- list(STOCK,ToSave1,ToSave2)
}
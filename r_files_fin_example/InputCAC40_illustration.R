# Function 'InputCAC40_illustration'
# Killian Martin--Horgassan
# 10-03-2015

# Manages the input for choice of the data used for the shares :
# low, high, closing or opening price.

InputCAC40_illustration <- function() {
cat("Choose the data to use :\n",
    "- [1] for the price at opening\n",
	"- [2] for the highest price\n",
	"- [3] for the lowest price\n",
	"- [4] for the price at the closing")
choice_dist <- as.character(readline("Your choice ? :\n"))
if (choice_dist == "1") {
	CHARACTER <- "3"
} else if (choice_dist == "2") {
	CHARACTER <- "4"
} else if (choice_dist == "3") {
	CHARACTER <- "5"
} else if (choice_dist == "4") {
	CHARACTER <- "6"
} else {
	stop("\n!! Invalid choice, try again !!\n")
}
output <- CHARACTER
}
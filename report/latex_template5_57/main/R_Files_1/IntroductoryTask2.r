# Master Thesis Project - Extreme Value Theory
# Introductory Task
# Killian Martin--Horgassan
# 19-02-2015

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Elementary test
#x <- 5
#print(x)

# sourcing works

# Loading function 'NormMax'
source("./NormMax2.r")
source("./InputIntroductoryTask2.r")

# Input from keyboard
DIST <- InputIntroductoryTask2()
DIST_1 <- DIST[[1]]
DIST_2 <- DIST[[2]]

# Calling function NormMax
listMax <- NormMax2(length(DIST_1),DIST_1,DIST_2)

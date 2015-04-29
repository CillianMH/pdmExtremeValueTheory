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
source("./NormMax.r")
source("./InputIntroductoryTask.r")

# Input from keyboard
DIST <- InputIntroductoryTask()
DIST_1 <- DIST[[1]]
DIST_2 <- DIST[[2]]

# Calling function NormMax
listMax <- NormMax(length(DIST_1),DIST_1,DIST_2)

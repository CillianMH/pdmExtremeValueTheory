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
source("/Users/kimartin/Desktop/R_files_pdm/NormMax.r")
source("/Users/kimartin/Desktop/R_files_pdm/InputIntroductoryTask.r")

# Calling function NormMax
#listMax <- NormMax()

# Input from keyboard
DIST <- InputIntroductoryTask()

# Calling function NormMax
listMax <- NormMax(length(DIST),DIST)

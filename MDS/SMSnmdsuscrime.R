# ------------------------------------------------------------------------------
# Book:         SMS
# ------------------------------------------------------------------------------
# Quantlet:     SMSnmdsuscrime
# ------------------------------------------------------------------------------
# Description:  SMSnmdsuscrime computes the nonmetric MDS U.S. crime data set
#               (uscrime.rda)
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       Plots of first and last iterations of nonmetric MDS for the original,
#               0-1 scaled and standardized U.S. health data set (ushealth.rda)
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Author:       Kristyna Sionova
# ------------------------------------------------------------------------------


# clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/MDS/")

# packages
# install.packages("MASS")
library(MASS)                               # load the MASS library

load("uscrime.rda")

UScrime.dist <- dist(uscrime, method = "manhattan")
UScrime.mds <- isoMDS(UScrime.dist)

plot(UScrime.mds$points, main="US crime", type = "n", ylab="Y", xlab="X")
title("US crime")
text(UScrime.mds$points,col=1+ as.numeric(uscrime$reg) ,labels = row.names(uscrime))

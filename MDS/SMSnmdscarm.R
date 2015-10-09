
# ------------------------------------------------------------------------------
# Book:         SMS
# ------------------------------------------------------------------------------
# Quantlet:     SMSnmdscarm
# ------------------------------------------------------------------------------
# Description:  SMSnmdscarm computes the 2D map of 4 cars based on ranks of
#               differences using nonmetric multidimensional scaling
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None
# ------------------------------------------------------------------------------
# Output:       2D map of 4 cars based on ranks of differences using nonmetric
#               multidimensional scaling
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Author:       Petra Cernayova
# ------------------------------------------------------------------------------

# clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()

# packages
# install.packages("MASS")
library(MASS)

# variables
desc = c("Nissan","Kia","BMW","Audi")
Dist = cbind(c(0,2,4,3),c(2,0,6,5),c(4,6,0,1),c(3,5,1,0))
Dist

# main calculation
b = isoMDS(Dist, k = 2, maxit = 1000)

# 2D map
plot(b$points,xlab="X",ylab="Y",xlim=c(-4,4),ylim=c(-4,4),main="NON-METRIC MULTIDIMENSIONAL SCALING",type="n")
text(b$points,desc,col="blue")


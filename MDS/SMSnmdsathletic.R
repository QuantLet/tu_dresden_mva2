# ------------------------------------------------------------------------------
# Book:         SMS
# ------------------------------------------------------------------------------
# Quantlet:     SMSnmdsathletic
# ------------------------------------------------------------------------------
# Description:  SMSnmdsathletic computes the nonmetric MDS for the athletic data
#               set (athletic.rda)
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       nonmetric MDS for the athletic data set (athletic.rda)
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Autor:        Radka Pickova
# ------------------------------------------------------------------------------

# clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/MDS/")

# load library
# install.packages("MASS")
library(MASS)

# load data
load("athletic.rda")

# compute distance, whole sample
athletic.dist = dist(athletic)

# nonmetric MDS
athletic.mds = isoMDS(athletic.dist)

# plot
plot(athletic.mds$points, main="Athletic records", type = "n", ylab="Y", xlab="X")
text(athletic.mds$points, labels = row.names(athletic))

# subsample without four most outlying countries
row.names(athletic)[c(12, 36, 38)]
x = as.matrix(athletic[c(1:11,13:35,37,39:54),]) # subsample
x = t(t(x)-matrix(rep(colMeans(x), dim(x)[1]), ncol = dim(x)[1]))%*%diag(1/sqrt(diag(var(x)))) # standardized distance matrix
athletic.dist = dist(x)

athletic.mds = isoMDS(athletic.dist)

# plot
plot(athletic.mds$points, main="Athletic records", type = "n", ylab="Y", xlab="X")
text(athletic.mds$points, labels = row.names(x))


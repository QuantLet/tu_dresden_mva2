# ------------------------------------------------------------------------------
# Book:         MVA
# ------------------------------------------------------------------------------
# Quantlet:     MVApcabankr
# ------------------------------------------------------------------------------
# Description:  MVApcabankr performs a PCA for the rescaled Swiss bank notes 
#               (bank2.dat). X1, X2, X3, X6 are taken in cm instead of mm. It 
#               shows the first three principal components in two-dimensional 
#               scatterplots. Additionally, a screeplot of the eigenvalues is 
#               displayed.
# ------------------------------------------------------------------------------
# keywords:     PCA, eigenvalues, scatterplot, screeplot
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       Two dimensional scatterplots of the first three principal
#               components of the rescaled Swiss bank notes data (bank2.dat). 
#               Additionally, a screeplot of the eigenvalues is displayed. 
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Author:       Zografia Anastasiadou
# ------------------------------------------------------------------------------

# Note: R decomposes matrices differently from Xplore (MVA book), and hence some
#       of the eigenvectors have different signs. This does not change the
#       results, but it does change the order of the graph by inverting the axes
#       around the origin (not always, and not necessarily all of the axis; it
#       depends on which eigenvectors we choose to plot).
#       In this case, the plots are inverted (compared with plots in the book). 

rm(list = ls(all = TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/PCA_D")

x = read.table("bank2.dat")
n = nrow(x)
x[, 1] = x[, 1] / 10
x[, 2] = x[, 2] / 10
x[, 3] = x[, 3] / 10
x[, 6] = x[, 6] / 10

colMeans(x)
# calculates eigenvalues and eigenvectors and sorts them by size
e  = eigen((n - 1) * cov(x) / n) 
e1 = e$values
# data multiplied by eigenvectors
x  = as.matrix(x) %*% e$vectors 

par(mfrow = c(2, 2))
# plot of the first vs. second PC
plot(x[, 1], x[, 2], pch = c(rep(1, 100), rep(3, 100)),
     col = c(rep("blue", 100), rep("red", 100)), 
     xlab = "PC1", ylab = "PC2", main = "First vs. Second PC",
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8)

# plot of the second vs. third PC
plot(x[, 2], x[, 3], pch = c(rep(1, 100), rep(3, 100)),
     col = c(rep("blue", 100), rep("red", 100)),
     xlab = "PC2", ylab = "PC3", main = "Second vs. Third PC",
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8)

# plot of the first vs. third PC
plot(x[, 1], x[, 3], pch = c(rep(1, 100), rep(3, 100)),
     col = c(rep("blue", 100), rep("red", 100)), 
     xlab = "PC1", ylab = "PC2", main = "First vs. Third PC",
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8)

# plot of the eigenvalues
plot(e1, ylim = c(0, 2.5), xlab = "Index", ylab = "Lambda",
     main = "Eigenvalues of S", cex.lab = 1.2, cex.axis = 1.2,
     cex.main = 1.8)
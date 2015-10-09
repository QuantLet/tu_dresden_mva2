# ------------------------------------------------------------------------------
# Book:         MVA
# ------------------------------------------------------------------------------
# Quantlet:     MVApcasimu
# ------------------------------------------------------------------------------
# Description:  MVApcasimu simulates a projection of a (normal) point cloud 
#               which may capture different proportions of the variance. One 
#               should vary nu to see the effects. (The chosen direction is 
#               always a linear combination of the first and second PCA 
#               direction.)
# ------------------------------------------------------------------------------
# Keywords:     PCA, eigenvalues, normal, principal components, scatterplot,
#				simulation
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       Projection of a (normal) point cloud which may capture different 
#               proportions of the variance. One should vary nu to see the 
#               effects. (The chosen direction is always a linear combination of
#               the first and second PCA direction.)
# ------------------------------------------------------------------------------
# Example:      nu  - 1
#               n   - 200
#               rho - 0.5 
#               sig - [1 0.5; 0.5 1]  
# ------------------------------------------------------------------------------
# Author:       Zografia Anastasiadou, Maria Osipenko
# ------------------------------------------------------------------------------


# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#setwd("C:/...") #Please change working directory

# install and load packages
#install.packages("mvtnorm")
#library(mvtnorm)

# set parameters
# set.seed(123)
n   = 1000
rho = 0.5
mu  = c(0, 0)

x = matrix(rnorm(n), ncol = 2)
sig = matrix(c(1, rho, rho, 1), nrow = 2)

nr = x %*% chol(sig) + mu
# nr  = rmvnorm(n, mu, sig)
eig = eigen(sig)
l   = eig$values
g   = eig$vectors
sh  = g %*% diag(sqrt(l)) %*% t(g)
x   = nr %*% t(sh)

direction1 = g[, 1]
direction2 = g[, 2]
norm       = c(t(direction1) %*% direction1, t(direction2) %*% direction2)
direction1 = direction1 / sqrt(norm[1])
direction2 = direction2 / sqrt(norm[2])

pd1 = sort(x %*% direction1)
d31 = pd1 %*% t(direction1)
pd2 = sort(x %*% direction2)
d32 = pd2 %*% t(direction2)
d4  = cbind(x[, 1], matrix(0, nrow = n))
d5  = cbind(x[, 2], matrix(0, nrow = n))

# plots of diection in simulated data and data projection with explained variance
dev.new()
par(mfrow = c(2, 1))
plot(x[, 1], x[, 2], , xlab = "", ylab="", main = "Direction in Data",
     cex.axis = 1.2, cex.main = 1.8)
lines(d31[, 1], d31[, 2], col = "red", lwd = 3)
plot(d4[, 1], d4[, 2], xlab = "", ylab = "", main = "Projection",
     cex.axis = 1.2, cex.main = 1.8)

varexp1  = var(pd1)
r1       = round(varexp1, 2)
w11      = toString(r1)
varexp2  = var(pd2)
r2       = round(varexp2,2)
w12      = toString(r2)
varsum   = sum(t(c(var(x[, 1]), var(x[, 2]))))
r2       = round(varsum, 2)
w2       = toString(r2)
varperc1 = varexp1 / varsum
r31      = round(varperc1, 2)
w31      = toString(r31)
varperc2 = varexp2 / varsum
r32      = round(varperc2, 2)
w32      = toString(r32)

mtext("Explained variance", side = 1, line = 2, at = -2, font = 2)
mtext(w11, side = 1, line = 2, at = -0.7, font = 2)
mtext("Total variance", side = 1, line = 3, at = -2.2, font = 2)
mtext(w2, side = 1, line = 3, at = -0.7, font = 2)
mtext("Explained percentage", side = 1, line = 4, at = -1.9, font = 2)
mtext(w31, side = 1, line = 4, at = -0.7, font = 2)

dev.new()
par(mfrow = c(2, 1))
plot(x[, 1], x[, 2], , xlab = "", ylab = "", main = "Direction in Data",
     cex.axis = 1.2, cex.main = 1.8)
lines(d32[, 1], d32[, 2], col = "red", lwd = 3)
plot(d5[, 1], d5[, 2], xlab = "", ylab = "", main = "Projection",
     cex.axis = 1.2, cex.main = 1.8)
mtext("Explained variance", side = 1, line = 2, at = -2, font = 2)
mtext(w12, side = 1, line = 2, at = -0.7, font = 2)
mtext("Total variance", side = 1, line = 3, at = -2.2, font = 2)
mtext(w2, side = 1, line = 3, at = -0.7, font = 2)
mtext("Explained percentage", side = 1, line = 4, at = -1.9, font = 2)
mtext(w32, side = 1, line = 4, at = -0.7, font = 2)


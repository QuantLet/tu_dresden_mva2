# ---------------------------------------------------------------------
# Book:         MVA
# ---------------------------------------------------------------------
# Quantlet:     MVAcanus
# ---------------------------------------------------------------------
# Description:  MVAcanus performs a canonical correlation analysis
#               for the US crime and US health data
# ---------------------------------------------------------------------
# Usage:        -
# ---------------------------------------------------------------------
# Inputs:       Data: uscrime.dat and ushealth.dat
# ---------------------------------------------------------------------
# Output:       Estimated covariance matrices Sxx, Sxy, Syx and Syy,
#               estimated matrix K and estimated canonical correlation
#               vectors (a and b) and canonical variables (eta and phi)
# ---------------------------------------------------------------------
# Example:      -
# ---------------------------------------------------------------------
# Author:       Dedy Dwi Prastyo 20110621
# ---------------------------------------------------------------------

graphics.off()		#close all windows
rm(list=ls(all=TRUE))  	# remove all variable from the workspace
# install.packages(c("tseries", "base")) #install and load library
# library(tseries)	
# library(base)  		

setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/Canonical/")


# Read US crime (X) and US health data (Y)

X <- read.table("uscrime.dat")
X <- as.matrix(X[ ,c(3,4,5,6,7,8,9)])

load("ushealth.rda");
Y <- as.matrix(ushealth[ ,c(4,5,6,7,8,9,10)])

# Estimation of covariance matrices

S   <- cov(cbind(X, Y))
Sxx <- S[1:dim(X)[2] , 1:dim(X)[2]]
Sxy <- S[1:dim(X)[2] , 1: dim(X)[2] + dim(Y)[2]]
Syx <- t(Sxy)
Syy <- S[1: dim(X)[2] + dim(Y)[2] , 1: dim(X)[2] + dim(Y)[2]]

# Estimation of the matrix K and its singular value decomposition

eigenX  <- eigen(Sxx)
eX  <- eigenX$values	# eigen velues of Sxx
vX  <- eigenX$vectors	# eigen vectors of Sxx

eigenY  <- eigen(Syy)
eY  <- eigenY$values	# eigen velues of Syy
vY  <- eigenY$vectors	# eigen vectors of Syy

K   <- vX %*% diag(1/(sqrt(eX))) %*% t(vX) %*% Sxy %*% vY %*%  diag(1/(sqrt(eY))) %*% t(vY) 
GLD <- svd(K)

G   <- GLD$u
L   <- diag(GLD$d)
D   <- GLD$v


# Estimated canonical correlation vectors (a and b) and canonical variables
# (eta and phi)

a <- vX %*% diag(1/ sqrt(eX)) %*% t(vX) %*% G
b <- vY %*% diag(1/ sqrt(eY)) %*% t(vY) %*% D

eta <- X %*% a
phi <- Y %*% b
plot(eta[,1], phi[,1], type = "n")
text(eta[,1], phi[,1], rownames(Y))
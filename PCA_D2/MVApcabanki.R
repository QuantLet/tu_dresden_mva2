# ------------------------------------------------------------------------------
# Book:         MVA
# ------------------------------------------------------------------------------
# Quantlet:     MVApcabanki
# ------------------------------------------------------------------------------
# Description:  MVApcabanki shows a screeplot of the eigenvalues for the PCA of 
#               the Swiss bank notes (bank2.dat). It computes the correlations 
#               between the variables and the principal components and displays 
#               the first two of them.
# ------------------------------------------------------------------------------
# See also:     MVAcpcaiv, MVAnpcabank, MVAnpcafood, MVAnpcahous, MVAnpcahousi, 
#               MVAnpcahousi, MVAnpcausco, MVAnpcausco2, MVApcabankr, MVApcasimu, 
#               MVApcasimu
# ------------------------------------------------------------------------------
# Keywords:     PCA, correlation, eigenvalues, principal components, screeplot
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Author:       Zografia Anastasiadou 20110106
# ------------------------------------------------------------------------------

 # clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/PCA_D2")
 # load data
x    = read.table("bank2.dat")
n    = nrow(x)
 # calculates eigenvalues and eigenvectors and sorts them by size
e    = eigen((n-1)*cov(x)/n) 
e1   = e$values/sum(e$values)

 # plot for the relative proportion of variance explained by PCs
dev.new()
plot(e1,ylim=c(0,0.8),xlab="Index",ylab="Variance Explained",main="Swiss Bank Notes",cex.lab=1.2,cex.axis=1.2,cex.main=1.8)

m    = apply(as.matrix(x),2,mean)
temp = as.matrix(x-matrix(m,n,ncol(x),byrow=T))
r    = temp%*%e$vectors
 # correlation between PCs and variables
r    = cor(cbind(r,x))
 # correlation of the two most important PCs and variables
r1   = r[7:12,1:2]

 # plot for the correlation of the original variables with the PCs
dev.new()
ucircle=cbind(cos((0:360)/180*pi),sin((0:360)/180*pi))
plot(ucircle,type="l",lty="solid",col="blue",xlab="First PC",ylab="Second PC",main="Swiss Bank Notes",cex.lab=1.2,cex.axis=1.2,cex.main=1.8,lwd=2)
abline(h=0.0,v=0.0)
label=c("X1","X2","X3","X4","X5","X6")
text(r1,label,cex=1.2)
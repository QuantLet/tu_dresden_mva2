# ------------------------------------------------------------------------------
# Book:         MVA
# ------------------------------------------------------------------------------
# Quantlet:     MVAnpcabank
# ------------------------------------------------------------------------------
# Description:  MVAnpcabank  performs a PCA for the standardized Swiss bank 
#               notes (bank2.dat) and shows the first three principal components 
#               in two-dimensional scatterplots. Additionally, a screeplot of 
#               the eigenvalues is displayed.
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       Two dimensional scatterplots of the first three principal 
#               components. 
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Author:       Zografia Anastasiadou 20110105
# ------------------------------------------------------------------------------

# Note: R decomposes matrices differently from Xplore (MVA book), and hence some
#       of the eigenvectors have different signs. This does not change the
#       results, but it does change the order of the graph by inverting the axes
#       around the origin (not always, and not necessarily all of the axis; it
#       depends on which eigenvectors we choose to plot).
#       In this case, the plots are inverted (compared with plots in the book). 

#clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/PCA_D2")

x<-read.table("bank2.dat")
#standardizes the data matrix
x<-scale(x)
#calculates eigenvalues and eigenvectors and sorts them by size
e<-eigen(cov(x))
e1<-e$values

#data multiplied by eigenvectors
x<-as.matrix(x)%*%e$vectors 

par(mfrow=c(2,2))
#plot of the first vs. second PC
plot(x[,1],x[,2],pch=c(rep(1,100),rep(3,100)),col=c(rep("blue",100),rep("red",100)),xlab="PC1",ylab="PC2",main="First vs. Second PC",cex.lab=1.2,cex.axis=1.2,cex.main=1.8)

#plot of the second vs. third PC
plot(x[,2],x[,3],pch=c(rep(1,100),rep(3,100)),col=c(rep("blue",100),rep("red",100)),xlab="PC2",ylab="PC3",main="Second vs. Third PC",cex.lab=1.2,cex.axis=1.2,cex.main=1.8)

#plot of the first vs. third PC
plot(x[,1],x[,3],pch=c(rep(1,100),rep(3,100)),col=c(rep("blue",100),rep("red",100)),xlab="PC1",ylab="PC2",main="First vs. Third PC",cex.lab=1.2,cex.axis=1.2,cex.main=1.8)

#plot of the eigenvalues
plot(e1,ylim=c(0,3),xlab="Index",ylab="Lambda",main="Eigenvalues of S",cex.lab=1.2,cex.axis=1.2,cex.main=1.8)
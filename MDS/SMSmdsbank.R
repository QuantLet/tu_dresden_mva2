# ------------------------------------------------------------------------------
# Book:         SMS
# ------------------------------------------------------------------------------
# Quantlet:     SMSmdsbank
# ------------------------------------------------------------------------------
# Description:  SMSmdsbank applies multidimensional scaling for swiss bank notes.
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       Plot of Metric MCS for Swiss bank notes and correlation of MDS
#               with original variables          
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Author:       
# ------------------------------------------------------------------------------

# clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()

# install packages
# install.packages("MASS")
library(MASS)
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/MDS/")

# load data
load("bank2.rda")

# factor variable
counter = rep(c("G","F"),each=100)

# main calculation
sol = cmdscale(dist(bank2))
corr = cor(cbind(sol,bank2))[3:8,1:2]

# plot
opar=par(mfrow=c(1,2))

# plot of coordinates
plot(sol,type="n",xlab="X",ylab="Y",main="Metric MDS")
text(sol,counter,col=rep(1:2,each=100))

# plot of correlations of mds with original variables
plot(c(-1.1,1.1),c(-1.1,1.1),type="n",main="Correlations of MDS/Variables",xlab="X",ylab="Y")
ucircle<-cbind(cos((0:360)/180*pi),sin((0:360)/180*pi))
points(ucircle,type="l",lty="dotted")
abline(h = 0)
abline(v = 0)
text(corr,labels=colnames(bank2),col="black")

par(opar)

# ------------------------------------------------------------------------------
# Book:         SMS
# ------------------------------------------------------------------------------
# Quantlet:     SMSdisfbank2
# ------------------------------------------------------------------------------
# Description:  SMSdisfbank2 reads the Swiss banknote data (bank2.dat) and 
#               spheres them to run Fisher's LDA and PC projection on them.
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       Fisher's LDA and PC projection plot
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# See also:     SMSsimpdsimu, SMSsimpdbank, SMSeppbank, SMSdisfbank2, SMSsiruscomp,
#               SMSsirsimu, SMSsir2simu, SMSsircars, SMSsir2cars, SMScartsq,
#               SMScartsq, SMScartdiag, SMSsvmorange, SMSsvmspiral, SMSsvmbankrupt
# ------------------------------------------------------------------------------
# Keywords:     discrimination, sphering, Fisher LDA projection, PC projection,
#               LDA, kernel smoothing, kernel, density, kde
# ------------------------------------------------------------------------------
# Author:       Edit: Awdesch Melzer 20140409
# ------------------------------------------------------------------------------
# Chapter:      20
# ------------------------------------------------------------------------------

# clear variables and close windows
 rm(list=ls(all=TRUE))
 graphics.off()
setwd("d:\\Private\\Lehre\\MVA2\\_excersises\\Discriminant\\") #set your working directory, create there a subdirectory 'data' for the datasets

# load libary
install.packages(c("MASS","KernSmooth"))
library(stats)
library(MASS)
library(KernSmooth)
# library(SMSdata)

# sphere data transform
sphere = function(x){
    x  = x - matrix(colMeans(x),nrow=NROW(x),ncol=NCOL(x),byrow=T)
    s  = svd(var(x))
    s  = s$u/matrix(sqrt(s$d),nrow(s$u),ncol(s$u),byrow=T)
    x  = as.matrix(x)%*%as.matrix(s)
return(x)
}

load("bank2.rda")

truth  = factor(rep(c("Genuine","Forged"),each=100)) # true data groups
x      = as.matrix(bank2)
x      = sphere(x)           # centering and sphering
h      = 2.62*NROW(x)^(-1/5) # bandwidth
fisher = as.matrix(x)%*%(lda(truth~x)$scaling) # Fisher's LDA
fisher = (fisher-mean(fisher))/sd(fisher)
principal = prcomp(x,scale.=TRUE)$x[,1]        # Principal Component

est.p  = bkde(principal, bandwidth=h, kernel="biweight")  # est. density using quartic kernel
est.f  = bkde(fisher, bandwidth=h, kernel="biweight")     # est. density using quartic kernel

dum    = rbind(c(min(c(est.p$x,est.f$x)),-0.2),c(max(c(est.p$x,est.f$x)),max(c(est.p$y,est.f$y))))

plot(dum,type="n",main="Fisher's LDA and PC projection",xlab="",ylab="")
lines(est.f,col="red", lwd=2)
lines(est.p,col="blue", lty=2, lwd=2)

t = (runif(200)*0.09)-0.095
points(t~fisher,col="red",pch=c(rep(1,100),rep(2,100)),cex=1)
t = (runif(200)*0.09)-0.195
points(t~principal,col="blue",pch=c(rep(1,100),rep(2,100)),cex=1)

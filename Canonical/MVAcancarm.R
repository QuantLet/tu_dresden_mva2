# ------------------------------------------------------------------------------
# Book:         MVA
# ------------------------------------------------------------------------------
# Quantlet:     MVAcancarm 
# ------------------------------------------------------------------------------
# Description:  MVAcancarm performs a canonical correlation analysis for the car 
#               marks data (carmean2.dat) and shows a plot of the first 
#               canonical variables.
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       Canonical correlation analysis for the car marks data 
#               (carmean2.dat) and shows a plot of the first 
#               canonical variables.
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Author:       Zografia Anastasiadou 20101213
# ------------------------------------------------------------------------------

rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/Canonical/")
#read data
cardat = read.table("carmean2.dat") 

#delete first column (names of the car marks)
car    = cardat[,-1]

#define variable names
colnames(car) = c("economy","service","value","price","design","sporty","safety","handling")  

#define car marks 
rownames(car) = c("Audi","BMW","Citroen","Ferrari","Fiat","Ford","Hyundai","Jaguar","Lada","Mazda","Mercedes","Mitsubishi","Nissan","Opel Corsa","Opel Vectra","Peugeot","Renault","Rover","Toyota","Trabant","VW Golf","VW Passat","Wartburg") 

#reordering the columns of the matrix
cars   = cbind(car[,4:3],car[,1:2],car[,5:8])
s      = cov(cars)

sa     = s[1:2,1:2]
sb     = s[3:8,3:8]

eiga   = eigen(sa)
eigb   = eigen(sb)

sa2    = eiga$vectors%*%diag(1/sqrt(eiga$values))%*%t(eiga$vectors)
sb2    = eigb$vectors%*%diag(1/sqrt(eigb$values))%*%t(eigb$vectors)

k      = sa2%*%s[1:2,3:8]%*%sb2
si     = svd(k)

a      = sa2 %*% si$u
b      = sb2 %*% si$v
eta    = as.matrix(cars[,1:2])%*%a[,1]
phi    = as.matrix(cars[,3:8])%*%b[,1]
etaphi = cbind(eta,phi)

plot(etaphi,type="n",xlab="Eta 1",ylab="Phi 1",main="Car Marks Data",cex.lab=1.2,cex.axis=1.2,cex.main=1.8)
text(etaphi,rownames(car))

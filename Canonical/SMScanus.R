## ----------------------------------------------------------------------------
##  Book:        Multivariate Statistics: Exercises and Solutions Series 
## ----------------------------------------------------------------------------
##	   
##
##  Macro:       SMScanus
## ----------------------------------------------------------------------------
##  Description: canonical correlation analysis for the US health and US crime data
## ----------------------------------------------------------------------------
##  Author:      Petra Cernayova
## ----------------------------------------------------------------------------
graphics.off()
rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/Canonical/")
load("ushealth.rda")
uscrime <- as.matrix(read.table("uscrime.dat"))         	# read a matrix; data(uscrime)
h<-ushealth[,3:11]
c<-uscrime[,3:9]
cca<-cancor(h,c)
u<-as.matrix(h) %*% cca$xcoef
v<-as.matrix(c) %*% cca$ycoef
op<-par(mfrow=c(2,3))
plot(u[,1],v[,1],xlab="HEALTH",ylab="CRIME",main="1ST CANONICAL VARIABLES",type="n", cex = 2)
text(u[,1],v[,1],row.names(ushealth),col="blue", cex = 2)
plot(u[,2],v[,2],xlab="HEALTH",ylab="CRIME",main="2ND CANONICAL VARIABLES",type="n")
text(u[,2],v[,2],row.names(ushealth),col="blue")
plot(u[,3],v[,3],xlab="HEALTH",ylab="CRIME",main="3RD CANONICAL VARIABLES",type="n")
text(u[,3],v[,3],row.names(ushealth),col="blue")
plot(u[,4],v[,4],xlab="HEALTH",ylab="CRIME",main="4TH CANONICAL VARIABLES",type="n")
text(u[,4],v[,4],row.names(ushealth),col="blue")
plot(u[,5],v[,5],xlab="HEALTH",ylab="CRIME",main="5TH CANONICAL VARIABLES",type="n")
text(u[,5],v[,5],row.names(ushealth),col="blue")
plot(u[,6],v[,6],xlab="HEALTH",ylab="CRIME",main="6TH CANONICAL VARIABLES",type="n")
text(u[,6],v[,6],row.names(ushealth),col="blue")
#plot(u[,7],v[,7],xlab="HEALTH",ylab="CRIME",main="7TH CANONICAL VARIABLES",type="n")
#text(u[,7],v[,7],row.names(ushealth),col="blue")
par(op)

# ------------------------------------------------------------------------------
# Book:         MVA
# ------------------------------------------------------------------------------
# Quantlet:     MVAcorrjourn
# ------------------------------------------------------------------------------
# Description:  MVAcorrjourn performs a correspondence analysis for the Belgian
#               journal data (journaux.dat), shows the eigenvalues of the 
#               singular value decomposition of the chi-matrix and displays 
#               graphically its factorial decomposition.
#               Corresponds to example 14.3 in MVA.
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None      
# ------------------------------------------------------------------------------
# Output:       Correspondence analysis for the Belgian journal data 
#               (journaux.dat), shows the eigenvalues of the singular value
#               decomposition of the chi-matrix and displays graphically its
#               factorial decomposition.
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Author:       Zografia Anastasiadou 20100502
# ------------------------------------------------------------------------------

# Note: R decomposes matrices differently from Xplore (MVA book).

rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/Correspondence/")

x<-read.table("journaux.dat")

a<-rowSums(x)
b<-colSums(x)

e<-matrix(a)%*%b/sum(a) 

#chi-matrix
cc<-(x-e)/sqrt(e)

#singular value decomposition
sv<-svd(cc)
g<-sv$u # left eigenvector
l<-sv$d # square root of eigenvalues
d<-sv$v # right eigenvector

#eigenvalues # eigen(as.matrix(cc) %*% t(as.matrix(cc)))$values
ll<-l*l
#cumulated percentage of the variance
aux<-cumsum(ll)/sum(ll)
perc<-cbind(ll,aux)

r1 <- matrix(l, nrow = nrow(g), ncol = ncol(g), byrow = T) * g #multiplies each column of g with each corresponding element of l
r  <- r1 / matrix(sqrt(a),nrow = nrow(g),ncol = ncol(g),byrow = F) #divides each row of r1 with each corresponding element of sqrt(a)

s1 <- matrix(l,nrow=nrow(d),ncol=ncol(d),byrow=T)*d #multiplies each column of d with each corresponding element of l
s  <- s1/matrix(sqrt(b),nrow=nrow(d),ncol=ncol(d),byrow=F) #divides each row of s1 with each corresponding element of sqrt(b)

car<-matrix(matrix(a),nrow=nrow(r),ncol=ncol(r),byrow=F)*r^2/matrix(l^2,nrow=nrow(r),ncol=ncol(r),byrow=T) #contribution in r

cas<-matrix(matrix(b),nrow=nrow(s),ncol=ncol(s),byrow=F)*s^2/matrix(l^2,nrow=nrow(s),ncol=ncol(s),byrow=T) #contribution in s

rr<-r[,1:2]
s2<-s[,1:2]

#labels for journals
types<-c("va","vb","vc","vd","ve","ff","fg","fh","fi","bj","bk","bl","vm","fn","fo")
#labels for regions
regions<-c("brw","bxl","anv","brf","foc","for","hai","lig","lim","lux")

rownames(s2) = rownames(cas) = regions
rownames(rr) = rownames(car) = types
    plot(rr,type="n",xlim=c(-1.1,1.5),ylim=c(-1.1,0.6),xlab="r_1,s_1",ylab="r_2,s_2",main="Journal Data",cex.axis=1.2,cex.lab=1.2,cex.main=1.6)
    points(s2,type="n")
    text(rr,types,cex=1.5,col="blue")
    text(s2,regions,cex = 2.0, col="red")
    abline(h=0,v=0,lwd=2)

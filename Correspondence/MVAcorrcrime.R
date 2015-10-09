# ------------------------------------------------------------------------------
# Book:         MVA
# ------------------------------------------------------------------------------
# Quantlet:     MVAcorrcrime
# ------------------------------------------------------------------------------
# Description:  MVAcorrcrime performs a correspondence analysis for the US crime
#               (uscrime.dat), shows the eigenvalues of the singular value 
#               decomposition of the chi-matrix and displays graphically its
#               factorial decomposition.
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Inputs:       None
# ------------------------------------------------------------------------------
# Output:       Correspondence analysis for the US crime (uscrime.dat), 
#               shows the eigenvalues of the singular value decomposition 
#               of the chi-matrix and displays graphically its factorial 
#               decomposition.
# ------------------------------------------------------------------------------
# Example:      -
# ------------------------------------------------------------------------------
# Author:       Zografia Anastasiadou 20100503
# ------------------------------------------------------------------------------

# Note: R decomposes matrices differently from Xplore, and
# hence some of the eigenvectors have different signs. This does not change
# the results, but it does change the order of the graph by inverting the
# axes around the origin (Not always, and not necessarily all of the axis;
# it depends on which eigenvectors we choose to plot)

rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/Correspondence/")

x1<-read.table("uscrime.dat")
x<-x1[,3:9]

a<-rowSums(x)
b<-colSums(x)

e<-matrix(a)%*%b/sum(a) 

#chi-matrix
cc<-(x-e)/sqrt(e)

#singular value decomposition
sv<-svd(cc)
g<-sv$u
l<-sv$d
d<-sv$v

#eigenvalues
ll<-l*l
#cumulated percentage of the variance
aux<-cumsum(ll)/sum(ll)
perc<-cbind(ll,aux)

#multiplies each column of g with each corresponding element of l
r1<-matrix(l,nrow=nrow(g),ncol=ncol(g),byrow=T)*g 
#divides each row of r1 with each corresponding element of sqrt(a)
r<-r1/matrix(sqrt(a),nrow=nrow(g),ncol=ncol(g),byrow=F) 
#multiplies each column of d with each corresponding element of l
s1<-matrix(l,nrow=nrow(d),ncol=ncol(d),byrow=T)*d 
#divides each row of s1 with each corresponding element of sqrt(b)
s<-s1/matrix(sqrt(b),nrow=nrow(d),ncol=ncol(d),byrow=F)

rr<-r[,1:2]
ss<-s[,1:2]

#labels for crimes
crime<-c("mur","rap","rob","ass","bur","lar","aut")

#labels for regions
state<-c("ME","NH","VT","MA","RI","CT","NY","NJ","PA","OH","IN","IL","MI","WI","MN","IA","MO","ND","SD","NE","KS","DE","MD","VA","VW","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA","OK","TX","MT","ID","WY","CO","NM","AZ","UT","NV","WA","OR","CA","AK","HI")

plot(rr,type="n",xlim=c(-0.4,0.6),ylim=c(-0.3,0.5),xlab="r_1,s_1",ylab="r_2,s_2",main="US Crime Data",cex.axis=1.2,cex.lab=1.2,cex.main=1.6)
points(ss,type="n")
text(rr,state,cex=1.2,col="blue")
text(ss,crime,col="red")
abline(h=0,v=0,lwd=2)

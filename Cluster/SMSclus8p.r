## ----------------------------------------------------------------------------
##  Book:        Multivariate Statistics: Exercises and Solutions Series 
## ----------------------------------------------------------------------------
##	   
##
##  Macro:       SMSclus8p
## ----------------------------------------------------------------------------
##  Description: Ward method and complete linkage using squared Euclidean 
##               distance for 8 points example
## ----------------------------------------------------------------------------
##  Author:      
## ----------------------------------------------------------------------------
#graphics.off()
rm(list=ls(all=TRUE))

eight=cbind(c(-3,-2,-2,-2,1,1,2,4),c(0,4,-1,-2,4,2,-4,-3))
eight=eight[c(8,7,3,1,4,2,6,5),]

opar <- par(mfrow = c(1,3))
plot(eight, type="n")
text(eight,as.character(1:8))

A = matrix(rep(t(eight), dim(eight)[1]), byrow = TRUE, ncol = 2)
B = matrix(rep(eight, each = dim(eight)[1]), ncol = 2)
p = 1
mydistance = matrix(rowSums(abs(A-B)^p)^(1/p), 8, 8)


plot(hclust(as.dist(mydistance),method="ward.D"),ylab=expression(L[p]),sub="",xlab="Ward method") 
plot(hclust(dist(eight,method="euclidean")^2,method="ward.D"),ylab="squared Euclidean distance",sub="",xlab="Ward method") 

plot(eight, type="n")
text(eight,as.character(1:8))

plot(hclust(dist(eight,method="euclidean")^2,method="complete"),ylab="squared Euclidean distance",sub="",xlab="Complete linkage") 
plot(hclust(dist(eight,method="euclidean")^2,method="single"),ylab="squared Euclidean distance",sub="",xlab="Single linkage") 

plot(hclust(dist(eight,method="manhattan"),method="single")) 

par(opar)


## ----------------------------------------------------------------------------
## Book:		SMS
## ----------------------------------------------------------------------------
## Quantlet:	SMSclusbank2
## ----------------------------------------------------------------------------
## Description:	SMSclusbank2 performs cluster analysis for 20 randomly chosen banknotes from the swiss banknotes dataset
## ----------------------------------------------------------------------------
## Usage:		cluster analysis using squared euclidean distance and single linkage
## ----------------------------------------------------------------------------
## Datafile:	bank2.rda 
## ----------------------------------------------------------------------------
## Author:		
## ----------------------------------------------------------------------------
## See also:	agglom tree distance
## ----------------------------------------------------------------------------
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/Cluster")

load("bank2.rda")

bank20.ind<-sample(1:200, 20, replace = FALSE)
twenty<-bank2[bank20.ind,]

library(MASS)  

prcomp2<-prcomp(twenty)$x[,1:2]	

biplot(prcomp(twenty, scale = TRUE),main="20 Swiss bank notes")  #principal component analysis

par(mfrow=c(2,2))	
plot(prcomp2,type="n",main="20 Swiss bank notes",xlab="first PC",ylab="second PC") 
text(prcomp2, labels = as.character(rownames(twenty)),col=as.numeric(as.numeric(rownames(twenty))<=100)+1)

hc <- hclust(dist(twenty,method="euclidean")^2,method="complete")
plot(hc,main="Dendrogram for 20 bank notes",xlab="Complete linkage")
hc2 <- hclust(dist(twenty,method="euclidean")^2,method="single")
plot(hc2,main="Dendrogram for 20 bank notes",xlab="Single linkage")
hc3 <- hclust(dist(twenty,method="euclidean")^2,method="ward.D2")
plot(hc3,main="Dendrogram for 20 bank notes",xlab="Ward")



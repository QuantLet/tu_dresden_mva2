## ----------------------------------------------------------------------------
## Book:		SMS
## ----------------------------------------------------------------------------
## Quantlet:	SMSnpcacrime
## ----------------------------------------------------------------------------
## Description:	normalized principal components for US crime data set
## ----------------------------------------------------------------------------
## Usage:		
## ----------------------------------------------------------------------------
## Datafile:	uscrime.rda
## ----------------------------------------------------------------------------
## Author:		
## ----------------------------------------------------------------------------
## See also:	
## ----------------------------------------------------------------------------
graphics.off()
rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/PCA_D2")

load("uscrime.rda")

pccrime=prcomp(~murder+rape+robbery+assault+burglary+larceny+autotheft,scale.=TRUE,data=uscrime)
print(pccrime)

## eigenvalues of the variance matrix
pccrime$sdev^2

opar=par(mfrow=c(2,2))
plot(pccrime)
biplot(pccrime)

plot(pccrime$x,type="n",main="First vs. second PC")
points(pccrime$x[,1:2],cex=1,col=1+as.numeric(uscrime$reg),pch=20+as.numeric(uscrime$reg))
text(pccrime$x[,1:2],row.names(uscrime),cex=0.7,col=1+as.numeric(uscrime$reg),pos=4)

plot(c(-1.1,1.1),c(-1.1,1.1),type="n",main="US crime",xlab="first PC",ylab="second PC") #plotting... [KONECNE!]
ucircle<-cbind(cos((0:360)/180*pi),sin((0:360)/180*pi))
points(ucircle,type="l",lty="dotted")
abline(h = 0)
abline(v = 0)
cors=pccrime$rotation%*%diag(pccrime$sdev)
lab=colnames(uscrime)[3:9]
text(cors,labels=lab,col="black")

par(opar)


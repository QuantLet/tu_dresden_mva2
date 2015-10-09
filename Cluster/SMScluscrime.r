## ----------------------------------------------------------------------------
## Book:		SMS
## ----------------------------------------------------------------------------
## Quantlet:	SMScluscrime
## ----------------------------------------------------------------------------
## Description:	SMScluscrime performs cluster analysis for US crime data set
## ----------------------------------------------------------------------------
## Usage:		
## ----------------------------------------------------------------------------
## Datafile:	uscrime.rda
## ----------------------------------------------------------------------------
## Author:		
## ----------------------------------------------------------------------------
## See also:	agglom tree distance
## ----------------------------------------------------------------------------
# graphics.off()
rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/Cluster")
load("uscrime.rda")
 
op=par(mfrow=c(1,2))
lab=paste(row.names(uscrime),uscrime$reg)
row.names(uscrime)=lab
x=uscrime[,3:9]
x=t(t(x)-apply(x,2,mean))
x=as.matrix(x)%*%diag(1/(apply(x,2,sd)))
# x = scale(x)
hc=hclust(dist(x),"ward.D2")
cl=cutree(hc,4)
names(cl)
plot(hc,labels=lab,main="Dendogram for US crime, Ward algorithm",xlab="",sub="")
pr=prcomp(x)
plot(pr$x[,1:2],type="n",main="US crime")
text(pr$x[,1:2],lab,col=as.numeric(cl)+1)
par(op)

## x is dataframe

x=data.frame(x)
colnames(x)=colnames(uscrime)[3:9]

## table of means in four clusters

sapply(x,tapply,cl,mean)

## ----------------------------------------------------------------------------
## Book:		SMS
## ----------------------------------------------------------------------------
## Quantlet:	SMSpcahealth
## ----------------------------------------------------------------------------
## Description:	principal components for US health data set
## ----------------------------------------------------------------------------
## Usage:		
## ----------------------------------------------------------------------------
## Datafile:	ushealth.rda
## ----------------------------------------------------------------------------
## Author:		
## ----------------------------------------------------------------------------
## See also:	
## ----------------------------------------------------------------------------
graphics.off()
rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/PCA_D2")
load("ushealth.rda")

pchealth=prcomp(~ acc+card+canc+pul+pneu+diab+liv,scale.=FALSE,data=ushealth)
print(pchealth)

## eigenvalues of the variance matrix are pchealth$sdev^2

opar=par(mfrow=c(2,2))
plot(pchealth)
biplot(pchealth)

plot(pchealth$x,type="n",main="First vs. second PC")
points(pchealth$x[,1:2],cex=1,col=1+as.numeric(ushealth$reg),pch=20+as.numeric(ushealth$reg))
text(pchealth$x[,1:2],row.names(ushealth),cex=0.7,col=1+as.numeric(ushealth$reg),pos=4)

plot(c(-1.1,1.1),c(-1.1,1.1),type="n",main="US health",xlab="first PC",ylab="second PC") #plotting... [KONECNE!]
ucircle<-cbind(cos((0:360)/180*pi),sin((0:360)/180*pi))
points(ucircle,type="l",lty="dotted")
abline(h = 0)
abline(v = 0)

only.nums = ushealth[,3:9]
cors = diag(1/sqrt(diag(var(as.matrix(ushealth[,3:9])))))%*%pchealth$rotation%*%diag(pchealth$sdev)
lab=colnames(ushealth)[3:9]
text(cors,labels=lab,col="black")

par(opar)


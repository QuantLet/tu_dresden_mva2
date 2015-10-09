rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/FA1")

x = as.matrix(read.table("bank2.dat"))
y=scale(x)
sigma = cov(y)


## Principal Components Methods
rm(list=ls(all=TRUE))

sigma=matrix(c(1,0.9,0.7,0.9,1,0.4,0.7,0.4,1),nrow=3)
eigen.all = eigen(sigma)

Q1 = sqrt(eigen.all$values[1]) * eigen.all$vectors[,1] # first Factor Loadings

Psi = diag(sigma - Q1 %*% t(Q1)) # variance of residuals

sum((sigma - (Q1 %*% t(Q1) + diag(Psi)))^2) 

## Principal Factors 
rm(list=ls(all=TRUE))

# sigma=matrix(c(1,0.9,0.7,0.9,1,0.4,0.7,0.4,1),nrow=3)


max.factor = 2
h.sq = apply(abs(sigma - diag(1, ncol(sigma))), 1, max)

for(i in 1:10){
    psi = 1 - h.sq
    eigen.all = eigen(sigma - diag(psi))
    m = if(i==1){sum(eigen.all$values>0)}else{min(max.factor, sum(eigen.all$values>0))}
    Q = eigen.all$vectors[,1:m] %*% diag(sqrt(eigen.all$values[1:m]), m, m)
    h.sq = rowSums(Q^2)
}

colnames(Q)<-c("Q1","Q2")
opar=par(mfrow=c(1,3))

lab<-colnames(x) #adding labels
plot(c(-1.1,1.1),c(-1.1,1.1),type="n",main="Swiss bank notes (loadings)",xlab="q1",ylab="q2") #plotting... [KONECNE!]
ucircle<-cbind(cos((0:360)/180*pi),sin((0:360)/180*pi))
points(ucircle,type="l",lty="dotted")
abline(h = 0)
abline(v = 0)
text(Q[,1:2],labels=lab,col="black")


theta=pi*7/12
rot=rbind(c(cos(theta),sin(theta)),c(-sin(theta),cos(theta)))

qr=Q%*%rot
plot(c(-1.1,1.1),c(-1.1,1.1),type="n",main="Swiss bank notes (rotation)",xlab="q1",ylab="q2") 
points(ucircle,type="l",lty="dotted")
abline(h = 0)
abline(v = 0)
text(qr[,1:2],labels=lab,col="black")

 
#### using R functions

facthle = factanal(x,factors=2,rotation="none",scores="none")

plot(c(-1.1,1.1),c(-1.1,1.1),type="n",main="Swiss bank notes (R function)",xlab="q1",ylab="q2") 
points(ucircle,type="l",lty="dotted")
abline(h = 0)
abline(v = 0)
text(facthle$loadings[,1:2],labels=lab,col="black")



par(opar)

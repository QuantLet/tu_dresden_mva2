## ----------------------------------------------------------------------------
## Book:		SMS
## ----------------------------------------------------------------------------
## Quantlet:	SMScluscomp
## ----------------------------------------------------------------------------
## Description:	SMScluscomp performs cluster analysis for US companies
## ----------------------------------------------------------------------------
## Usage:		
## ----------------------------------------------------------------------------
## Datafile:	uscomp.rda
## ----------------------------------------------------------------------------
## Author:		Jakub Pecanka
## ----------------------------------------------------------------------------
## See also:	agglom tree distance grrot
## ----------------------------------------------------------------------------
# graphics.off()
rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/Cluster")

 load("uscomp.rda")
 uscomp$Sales=as.numeric(as.character(uscomp$Sales))
 uscomp$Sales[65]=1601

 xx=as.matrix(uscomp[,-7]) # remove sector
 xxi=substr(as.character(uscomp$Sector),1,3)
 xxxi=row.names(uscomp)
 n=nrow(xx)                            # sample size

 meanxx=apply(xx,2,mean) # or colMeans
 x=t(t(xx)-meanxx)
 minxx=apply(xx,2,min)
 maxxx=apply(xx,2,max)
 varxx=apply(xx,2,var)
 xxp=log(t(t(xx)-minxx+(maxxx-minxx)/200))
 xxp=round(xxp,4)


prc = prcomp(xxp)
biplot(prc,main="Companies")  #principal component analysis
y2 = prc$x[,c(1,2)]

# second part
 d=dist(xxp,method="manhattan")
 t=hclust(d,method="ward.D")

 label=uscomp$Sector
 label1=sapply(label,substr,1,2)
# show the dendrogram 
 g=plot(t,main="Dendrogram for US companies, Ward algorithm",labels=label1)
 
  cl=cutree(t,8)

# show the clusters 
 plot(y2, main="Five Clusters for US Companies",xlab="first PC",ylab="second PC",type="n")
 text(matrix(y2[label1=="Co"],ncol=2),labels="Co",col="navy")
 text(matrix(y2[label1=="En"],ncol=2),labels="En",col="blue")
 text(matrix(y2[label1=="Fi"],ncol=2),labels="Fi",col="green")
 text(matrix(y2[label1=="Hi"],ncol=2),labels="Hi",col="red")
 text(matrix(y2[label1=="Ma"],ncol=2),labels="Ma",col="black")
 text(matrix(y2[label1=="Ot"],ncol=2),labels="Ot",col="brown")
text(matrix(y2[label1=="Re"],ncol=2),labels="Re",col="orange")
text(matrix(y2[label1=="Tr"],ncol=2),labels="Tr",col="purple")
par(opar)

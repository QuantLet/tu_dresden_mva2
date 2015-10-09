## ----------------------------------------------------------------------------
## Book:		SMS
## ----------------------------------------------------------------------------
## Quantlet:	SMSdisfwais
## ----------------------------------------------------------------------------
## Description:	Fisher's LDA for WAIS data (apparent and actual error rate)
## ----------------------------------------------------------------------------
## Usage:		
## ----------------------------------------------------------------------------
## Datafile:	wais.rda
## ----------------------------------------------------------------------------
## Author:		
## ----------------------------------------------------------------------------
## See also:	mean cov inv sum denest setmaskp setmaskl createdisplay show setgopt
## ----------------------------------------------------------------------------
graphics.off()
rm(list=ls(all=TRUE))
setwd("d:\\Private\\Lehre\\MVA2\\_excersises\\Discriminant\\") #set your working directory, create there a subdirectory 'data' for the datasets
library(stats)
library(MASS)

load("wais.rda")

fisher.w=lda(group~.-subject,prior=c(0.5,0.5),data=wais)
prediction=predict(fisher.w, wais)$class

t=table(wais$group,prediction)

print(t)
aper=(sum(t)-sum(diag(t)))/sum(t)
print(aper)

correct=0

for (i in 1:nrow(wais)) {
  fisher.t=lda(group~.-subject,prior=c(0.5,0.5),subset=-i,data=wais)
  predict=predict(fisher.t, wais[i,])$class
  if (predict==wais[i,"group"]) correct=correct+1
}

aer=1-correct/nrow(wais)
print(aer)

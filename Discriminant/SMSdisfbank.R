## ----------------------------------------------------------------------------
## Book:		SMS	
## ----------------------------------------------------------------------------
## Quantlet:	SMSdisfbank
## ----------------------------------------------------------------------------
## Description:	Fisher's LDA for 20 bank notes
## ----------------------------------------------------------------------------
## Usage:		
## ----------------------------------------------------------------------------
## Datafile:	bank2.rda
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

load("bank2.rda")

keep=sample(1:200,20,replace=FALSE)
                                        # 20 randomly selected Swiss bank notes
#keep=c(7,8,16,39,71,73,89,94,94,100,110,121,129,131,149,150,154,161,163,174)
                                        # uncomment for 20 banknotes in the book
bank<-bank2[keep,]                   
truth=factor(rep(c("Genuine","Forged"),each=100))
group=truth[keep]

fisher=lda(bank,grouping=group)
prediction=predict(fisher, bank2)$class

table(truth,prediction)


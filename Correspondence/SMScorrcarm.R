# ----------------------------------------------------------------------------
# Book:		     SMS
# ----------------------------------------------------------------------------
# Quantlet:	     SMScorrcarm
# ----------------------------------------------------------------------------
# Description:   SMScorrcarm performs a correspondence analysis for the Car data
#                ("carmean2.rda"), shows the eigenvalues of the singular value
#                decomposition of the chi-matrix and displays graphical its
#                factorial decomposition. 
# ----------------------------------------------------------------------------
# Usage:		-
# ----------------------------------------------------------------------------
# Datafile:     carmean2.rda 
# ----------------------------------------------------------------------------
# Author:       Radka Pickova, Zdenek Hlavka
# ----------------------------------------------------------------------------
# See also:	    SMScorrcarm, SMSchi2bac, SMScorrcrime, SMScorrhealth, SMScorrfood
# ----------------------------------------------------------------------------
# Keywords:     correspondence analysis, SVD, singular value decomposition,
#               factorial decomposition
# ----------------------------------------------------------------------------
# Chapter:      15
# ----------------------------------------------------------------------------

 # clear variables and close windows
rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/Correspondence/")
library(ca)   # open package
library(MASS)

load("carmean2.rda") # load carmean2 data

# Package "MASS"
corrcar   = corresp(carmean2 * 10,nf = 3) 
plot(corrcar)
print(corrcar) # printing results

# Package "ca"
corrcarm  = ca(carmean2, nd = NA)
plot(corrcarm)
print(corrcarm)

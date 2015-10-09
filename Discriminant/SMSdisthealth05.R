# ----------------------------------------------------------------------------
# Book:		    SMS
# ----------------------------------------------------------------------------
# Quantlet:	    SMSdisthealth05
# ----------------------------------------------------------------------------
# Description:	SMSdisthealth05 calculates distance matrices for Maine, New
#               Hampshire and New York from the US health 2005 data set.
#               The distance measures are Euclidean, Manhattan and maximum distance.
# ----------------------------------------------------------------------------
# Usage:		-
# ----------------------------------------------------------------------------
# Datafile:	    ushealth05.csv
# ----------------------------------------------------------------------------
# See also:	    SMSclus8p, SMSclus8pd, SMSclusbank, SMSclusbank2,
#              SMSclusbank3, SMScluscomp, SMScluscrime, SMScluscrimechi2,
#              SMSclushealth, MVAclus8p, MVAclusbank, MVAclusbh, MVAclusfood,
#              MVAsimcar
# ------------------------------------------------------------------------------
# Keywords:     cluster analysis, distance, Euclidean, Manhattan, Maximum
# ----------------------------------------------------------------------------
# Author:       Awdesch Melzer
# ----------------------------------------------------------------------------


# clear cache and close windows
rm(list=ls(all=TRUE))
graphics.off()

setwd("d:\\Private\\Lehre\\MVA2\\_excersises\\Discriminant\\") #set your working directory, create there a subdirectory 'data' for the datasets

ushealth05        = read.csv("ushealth05.csv",sep=",",header=T) # load ushealth data

ush               = ushealth05[order(ushealth05[,20]),] # order data
ushreg            = as.numeric(ush[,20])                # def. regrion
lab               = paste(ush[,22],ushreg)
row.names(ush)    = lab
ush        = ush[c(which(ush$ANSI==c("ME")),which(ush$ANSI==c("NH")),which(ush$ANSI==c("NY"))),]     # use only Maine, New Hampshire, New York
ush        = ush[,3:12] # for the disease related death causes

# Euclidean distance
dist.eu    = dist(ush,method="euclidean",p=2,diag=T)
dist.eu
# Manhattan distance
dist.ma    = dist(ush,method="manhattan",p=2,diag=T)
dist.ma
# Maximum distance
dist.mi    = dist(ush,method="maximum",p=2,diag=T)
dist.mi

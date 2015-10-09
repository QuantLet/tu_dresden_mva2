# ------------------------------------------------------------------------------
# Book:         MVA
# ------------------------------------------------------------------------------
# Quantlet:     MVAboxcar
# ------------------------------------------------------------------------------
# Description:  MVAboxcar computes Boxplots for the mileage (Variable 14) 
#               of US, Japanese and European cars from carc.txt, 
#               respectively.
# ------------------------------------------------------------------------------
# Usage:        -
# ------------------------------------------------------------------------------
# Keywords:     boxplot, quantile
# ------------------------------------------------------------------------------
# Inputs:       none
# ------------------------------------------------------------------------------
# Output:       Boxplots for the mileage (Variable 14) of US, Japanese 
#               and European cars from carc.dat,respectively.
# ------------------------------------------------------------------------------
# Example:     
# ------------------------------------------------------------------------------
# Author:       Wolfgang Haerdle, Jorge Patron, Vladimir Georgescu, 
#               Song Song
# ------------------------------------------------------------------------------

# close windows, clear variables
rm(list = ls(all = TRUE))
graphics.off()

# setwd("/Users/...")    # set working directory if mac
setwd("d:\\Private\\Lehre\\MVA2\\_excersises\\ComparisonOfBatches\\")
x      = read.table("carc.txt") # reads data

us     = x[which(x[,13] == 1),2]
japan  = x[which(x[,13] == 2),2]
europe = x[which(x[,13] == 3),2]

m1 = mean(us)
m2 = mean(japan)
m3 = mean(europe)

boxplot(us, japan, europe, axes = FALSE, frame = TRUE)
axis(side = 1, at = seq(1, 3), label = c("US", "JAPAN", "EU") )
axis(side = 2, at = seq(0, 50, 5), label = seq(0, 50, 5))
title('Car Data')
lines(c(0.6, 1.4), c(m1, m1), lty = "dotted", lwd = 1.2)
lines(c(1.6, 2.4), c(m2, m2), lty = "dotted", lwd = 1.2)
lines(c(2.6, 3.4), c(m3, m3), lty = "dotted", lwd = 1.2)

five = quantile(x[, 2], c(.025, .25, .50, .75, .975));
five

# ---------------------------------------------------------------------
# Book:         MVA
# ---------------------------------------------------------------------
# Quantlet:     MVAparcoo1
# ---------------------------------------------------------------------
# Description:  MVAparcoo1 computes a parallel coordinate plot for the
#               observations 96-105 of the Swiss bank notes data
#               (bank2.dat).
# ---------------------------------------------------------------------
# Usage:        -
# ---------------------------------------------------------------------
# Inputs:       None
# ---------------------------------------------------------------------
# Output:       Parallel coordinate plot for the observations 96-105 of
#               the Swiss bank notes data (bank2.dat).
# ---------------------------------------------------------------------
# Example:      -
# ---------------------------------------------------------------------
# Author:       Wolfgang Haerdle, Michal Benko, Vladimir Georgescu,
#               Jorge Patron, Song Song, Julia Wandke 2006-09-19
# ---------------------------------------------------------------------

rm(list=ls(all=TRUE))
graphics.off()
 
# Load data
# The data file should be located in the same folder as this Qlet
# Set the R working directory to this directory using setwd()
setwd("d:\\Private\\Lehre\\MVA2\\_excersises\\ComparisonOfBatches\\")
x = read.table("bank2.dat")
xpart = x[96:105,]

### parallel Coordinates

install.packages("MASS")
library(MASS)

parcoord(xpart, lwd =2, 
    col = c(1,1,1,1,1,2,2,2,2,2), lty=c(rep(1,5),rep(4,5)), 
    main="Parallel coordinates plot (Bank data)", frame=TRUE)
axis(side=2, at=seq(0,1,0.2), labels=seq(0,1,0.2))

### Flury faces

install.packages("aplpack") 
library(aplpack)

xpart = x[91:110,]
ncolors=15

faces(xpart, nrow = 4,face.type=1,scale=TRUE, col.nose = rainbow(ncolors), col.eyes = rainbow(ncolors, 
    start = 0.6, end = 0.85), col.hair = terrain.colors(ncolors), 
    col.face = heat.colors(ncolors), col.lips = rainbow(ncolors, 
    start = 0, end = 1), col.ears = rainbow(ncolors, start = 0, 
    end = 0.8), plot.faces = TRUE)

####


nr avtpobusa AA5472CT
tel 015737554481
vidpravka 16:30, hbf.

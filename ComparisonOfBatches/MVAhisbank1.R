# ---------------------------------------------------------------------
# Book:         MVA
# ---------------------------------------------------------------------
# Quantlet:     MVAhisbank1
# ---------------------------------------------------------------------
# Description:  MVAhisbank1 computes 4 histograms for the diagonal of 
#               the forged swiss bank notes (bank2.dat). The 
#               histograms are different w.r.t. their binwidth. 
# ---------------------------------------------------------------------
# Usage:        -
# ---------------------------------------------------------------------
# Inputs:       None
# ---------------------------------------------------------------------
# Output:       Computes 4 histograms for the diagonal of the forged 
#               swiss bank notes (bank2.dat). The histograms are
#               different w.r.t. their binwidth. 
# ---------------------------------------------------------------------
# Example:      -
# ---------------------------------------------------------------------
# Author:       Wolfgang Haerdle, Song Song, Vladimir Georgescu, 
#               Jorge Patron           
# ---------------------------------------------------------------------

# clear variables and close windows

rm(list=ls(all=TRUE))
graphics.off()

# Load data
# The data file should be located in the same folder as this Qlet
# Set the R working directory to this directory using setwd()
# setwd("C:/...")        # set working directory if windows
# setwd("/Users/...")    # set working directory if macx = read.table('bank2.dat');      
setwd("d:\\Private\\Lehre\\MVA2\\_excersises\\ComparisonOfBatches\\")
x = read.table('bank2.dat');
x = x[101:200,6]; # only diagonal

origin = 137.75; # origin of the model (where it starts)
#origin1 = 137.65;
#origin2 = 137.75;
#origin3 = 137.85;
#origin4 = 137.95;

# Because origin<min(x), the histogram includes all values
y1 = seq(origin,141.05,0.1) # sequence of points from origin till 141.05 with step 0.1 (will be our grid for histogram)
y2 = seq(origin,141.05,0.2)
y3 = seq(origin,141.05,0.3)
y4 = seq(origin,141.05,0.4)

par(mfrow=c(2,2)) # split the plotting screen

hist(x,y1, ylab="Diagonal", xlab="h = 0.1", xlim=c(137.5,141), ylim=c(0,10.5), main="Swiss Bank Notes")
#axis(side=1, at=seq(138,141), labels=seq(138,141)) # if no axes
#axis(side=2, at=seq(0,10,2),labels=seq(0,10,2))    # if no axes
hist(x,y3, ylab="Diagonal", xlab="h = 0.3", xlim=c(137.5,141), ylim=c(0,31.5), main="Swiss Bank Notes")
hist(x,y2, ylab="Diagonal", xlab="h = 0.2", xlim=c(137.5,141), ylim=c(0,21), main="Swiss Bank Notes")
hist(x,y4, ylab="Diagonal", xlab="h = 0.4", xlim=c(137.5,141), ylim=c(0,42), main="Swiss Bank Notes")


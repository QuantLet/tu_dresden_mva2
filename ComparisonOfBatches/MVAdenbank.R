# ---------------------------------------------------------------------
# Book:         MVA
# ---------------------------------------------------------------------
# Quantlet:     MVAdenbank
# ---------------------------------------------------------------------
# Description:  MVAdenbank computes kernel density estimates of the 
#               diagonal of the genuine and forged swiss bank notes 
#               (bank2.dat). The bandwidth parameter are chosen by 
#               Silverman's rule of thumb.      
# ---------------------------------------------------------------------
# Usage:        -
# ---------------------------------------------------------------------
# Inputs:       none
# ---------------------------------------------------------------------
# Output:       Kernel density estimates of the diagonal of the genuine 
#               and forged swiss bank notes (bank2.dat).      
# ---------------------------------------------------------------------
# Example:      
# ---------------------------------------------------------------------
# Author:        Wolfgang Haerdle, Michal Benko, Vladimir Georgescu, 
#               Jorge Patron, Song Song
# ---------------------------------------------------------------------

rm(list=ls(all=TRUE))
graphics.off()
setwd("d:\\Private\\Lehre\\MVA2\\_excersises\\ComparisonOfBatches\\")

# load data

install.packages(c("KernSmooth", "graphics", "misc3d", "lattice"))
library(graphics)
x = read.table('bank2.dat');     # read data 

#### KDE 1dim
# plot(density(x[,6])) # very nice function

x1 = x[1:100,6];# diagonal of genuine
x2 = x[101:200,6]; # diagonal of counterfeit

fh1 = bkde(x1, kernel="biweight")  #Compute kernel density estimate
fh2 = bkde(x2, kernel="biweight")  #Compute kernel density estimate

plot(fh1,type="l", lwd=2,xlab="Counterfeit                  /                 Genuine",ylab="Density estimates for diagonals", col="black", main="Swiss bank notes", xlim=c(137,143), ylim=c(0,0.85)) #Plot KDE og Genuine
lines(fh2, lty="dotted", lwd=2, col="red3") # plot of counterfeit

#### scattetplot 2d

pchs = c(rep(19, 100), rep(21, 100))
cols = c(rep("red3", 100), rep("blue3", 100))
plot(x[,5], x[,6], pch=pchs, col=cols, type = "p", frame=TRUE, main = "Swiss bank notes")

#### KDE 2d

library(KernSmooth)
d  = bkde2D(x[, 5:6], bandwidth = 1.06*c(sd(x[, 5]), sd(x[, 6]))* 200^(-1/5)) # computes the kernel smoother
contour(d$x1, d$x2, d$fhat, xlim = c(8.5, 12.5), ylim = c(137.5, 143), col = c("blue", "black", "yellow", "cyan", "red", "magenta", "green", "blue", "black"), lwd=3, cex.axis = 1)

#### Draftmens Plot

pairs(x, col = cols, pch = pchs) # wonderful function !

#### scatterplot 3d

library(lattice)
cloud(x[,6]~x[,5]*x[,4],pch=pchs, col=cols, ticktype="detailed",
	main=expression(paste("Swiss bank notes")),
	screen=list(z=-90,x=-90, y=45),
#	scales=list(arrows=FALSE,col="black",distance=1,tick.number=c(4,4,5),cex=.7,
#		z=list(labels=round(seq(138,142,length=6))),
#		x=list(labels=round(seq(7,14,length=6))),
#		y=list(labels=round(seq(7,12,length=6)))
#		),
	xlab=list(expression(paste("Lower inner frame (X4)")),rot=-10,cex=1.2),
	ylab=list("Upper inner frame (X5)",rot=10,cex=1.2),
	zlab=list("Diagonal (X6)", rot=90,cex=1.1))

#### KDE 3d 

library(misc3d)
d   = kde3d(x[, 4], x[, 5], x[, 6], n = 15)
contour3d(d$d, level = c(max(d$d[10,10,])*.02, max(d$d[10,10,])*.5, max(d$d[10,10,])*1.3), fill = c(FALSE, FALSE, TRUE), col.mesh = c("green","red","blue") , engine = "standard", screen=list(z=210,x=-40,y=-295), scale=TRUE)


## ----------------------------------------------------------------------------
## Book:		    SMS
## ----------------------------------------------------------------------------
## Quantlet:	  SMSfactuscrime
## ----------------------------------------------------------------------------
## Description:	factor analysis of the US crime data set and show plots of
##              the factor loading.
## ------------------------------------------------------------------------------
## Keywords:    factor, factor analysis, factor loadings, uscrime
## ----------------------------------------------------------------------------
## Usage:		
## ----------------------------------------------------------------------------
## Datafile:	  uscrime.rda
## ----------------------------------------------------------------------------
## Author:		
## ----------------------------------------------------------------------------
## See also:	
## ----------------------------------------------------------------------------
graphics.off()
rm(list = ls(all = TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/FA")

# attach("uscrime.rda")
load("uscrime.rda")

factcrime = factanal(~ murder + rape + robbery + assault + burglary + larceny + autotheft, factors = 3, rotation = "varimax", scores = "regression", data = uscrime)
print(factcrime)

lab = colnames(uscrime)[3:9]

opar = par(mfrow = c(2, 2))
plot(c(-1.1, 1.1), c(-1.1, 1.1), type = "n", main = "US crime (varimax)", xlab = "q1", ylab = "q2")
ucircle = cbind(cos((0:360) / 180 * pi), sin((0:360) / 180 * pi))
points(ucircle, type = "l", lty = "dotted")
abline(h = 0)
abline(v = 0)
text(factcrime$loadings[, 1:2], labels = lab, col = "black")

plot(c(-1.1, 1.1), c(-1.1, 1.1), type = "n", main = "US crime (varimax)", xlab = "q3", ylab = "q2")
points(ucircle, type = "l", lty = "dotted")
abline(h = 0)
abline(v = 0)
text(factcrime$loadings[, c(3, 2)], labels = lab, col = "black")

plot(c(-1.1, 1.1), c(-1.1, 1.1), type = "n", main = "US crime (varimax)", xlab = "q1", ylab = "q3")
points(ucircle, type = "l", lty = "dotted")
abline(h = 0)
abline(v = 0)
text(factcrime$loadings[, c(1, 3)], labels = lab, col = "black")


par(mfrow = c(2, 2), ask = dev.interactive(orNone = TRUE))

plot(factcrime$scores[, 1:2], type = "n", xlab = "q1", ylab = "q2", main = "US crime (factor scores)")
points(factcrime$scores[, 1:2], cex = 0.8, col = 1 + as.numeric(uscrime$reg), pch = 20 + as.numeric(uscrime$reg))
text(factcrime$scores[, 1:2], row.names(uscrime), cex = 0.5, col = 1 + as.numeric(uscrime$reg), pos = 4)
plot(factcrime$scores[, c(3,2)], type = "n", xlab = "q3", ylab = "q2", main = "US crime (factor scores)")
points(factcrime$scores[, c(3, 2)], cex = 0.8, col = 1 + as.numeric(uscrime$reg), pch = 20 + as.numeric(uscrime$reg))
text(factcrime$scores[, c(3, 2)], row.names(uscrime), cex = 0.5, col = 1 + as.numeric(uscrime$reg), pos = 4)
plot(factcrime$scores[, c(1, 3)], type = "n", xlab = "q1", ylab = "q3", main = "US crime (factor scores)")
points(factcrime$scores[, c(1, 3)], cex = 0.8, col = 1 + as.numeric(uscrime$reg), pch = 20 + as.numeric(uscrime$reg))
text(factcrime$scores[, c(1, 3)], row.names(uscrime), cex = 0.5, col = 1 + as.numeric(uscrime$reg), pos = 4)

par(opar)


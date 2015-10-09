# ----------------------------------------------------------------------------
# Book:		    SMS
# ----------------------------------------------------------------------------
# Quantlet:		SMSfactanal
# ----------------------------------------------------------------------------
# Description:	factor analysis of athletic records and show plots of
#              	the factor loading.
# ------------------------------------------------------------------------------
# Keywords:    	factor, factor analysis, factor loadings, athletic
# ----------------------------------------------------------------------------
# Usage:		-
# ----------------------------------------------------------------------------
# Datafile:	  	athletic.rda
# ----------------------------------------------------------------------------
# Author:		-
# ----------------------------------------------------------------------------
# See also:		-
# ----------------------------------------------------------------------------


rm(list=ls(all=TRUE))
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/FA1")
load("athletic.rda")

facthle = factanal(athletic, factors = 3, rotation = "varimax", scores = "regression")
print(facthle)

lab = colnames(athletic)


opar = par(mfrow = c(2, 2))
plot(c(-1.1, 1.1), c(-1.1, 1.1), type = "n", main = "Athletic (varimax)", xlab = "q1", ylab = "q2")
ucircle = cbind(cos((0:360) / 180 * pi), sin((0:360) / 180 * pi))
points(ucircle, type = "l", lty = "dotted")
abline(h = 0)
abline(v = 0)
text(facthle$loadings[, 1:2], labels = lab, col = "black")

plot(c(-1.1, 1.1), c(-1.1, 1.1), type = "n", main = "Athletic (varimax)", xlab = "q3", ylab = "q2")
points(ucircle, type = "l", lty = "dotted")
abline(h = 0)
abline(v = 0)
text(facthle$loadings[, c(3, 2)], labels = lab, col = "black")

plot(c(-1.1, 1.1), c(-1.1, 1.1), type = "n", main = "Athletic (varimax)", xlab = "q1", ylab = "q3")
points(ucircle, type = "l", lty = "dotted")
abline(h = 0)
abline(v = 0)
text(facthle$loadings[, c(1, 3)], labels = lab, col = "black")


par(mfrow = c(2, 2))

plot(facthle$scores[, 1:2], type = "n", xlab = "q1", ylab = "q2", main = "Athletic (factor scores)")
text(facthle$scores[, 1:2], row.names(athletic), cex = 1)
plot(facthle$scores[, c(3, 2)], type = "n", xlab = "q3", ylab = "q2", main = "Athletic (factor scores)")
text(facthle$scores[, c(3, 2)], row.names(athletic), cex = 1)
plot(facthle$scores[, c(1, 3)], type = "n", xlab = "q1", ylab = "q3", main = "Athletic (factor scores)")
text(facthle$scores[, c(1, 3)], row.names(athletic), cex = 0.5)

par(opar)

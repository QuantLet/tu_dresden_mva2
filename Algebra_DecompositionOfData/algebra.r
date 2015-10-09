a1 = c(1, 2, 3)
a1
a2 = 1:3
a2
a3 = seq(1, 3, by = 1)
a3
class(a3)
t(a3)
t(t(a3))
2 * a1
v = c(4, 5, 6)
v
a1 + v
a1 * v
a1 %*% v # scalar product
t(t(a1)) %*% v # matrix
outer(a1, v, "*")

b = cbind(c(1, 2),c(2, 1))
b
det(b)
solve(b)
solve(b) * 3

## needs rights to install packages !
install.packages("MASS")
library("MASS")
ginv(b) # generalized inverse ! (if usual inevrse exists, then it is equal to it)
d = cbind(c(1,1),c(1,1))/2
det(d)
solve(d)
ginv(d) 
ginv(d) %*% d
##
gd = matrix(rep(0.5, 4), 2, 2) # idempotent
gd
gd %*% gd
gd %*% gd %*% gd %*% gd

rank(b) # not the matrix Rank !!!
trace(b) # not the matrix trace !!!
diag(b)
sum(diag(b))
eigen(b) # eigenvalues and eigenvectors
eigen(b)$vectors * sqrt(2)

lambda = diag(eigen(b)$values)
gamma  = eigen(b)$vectors

eigen(b)$vectors %*% t(eigen(b)$vectors) # eigenvectors orthogonal

gamma %*% lambda %*% t(gamma) # spectral decomposition !!!
b



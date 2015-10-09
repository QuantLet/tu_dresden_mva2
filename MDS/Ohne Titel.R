rm(list = ls())
library("nortest")
library(KernSmooth)
setwd("/Users/ookhrin/Documents/Private/IdeaDescription/Yandex/")

p.vals = read.table("Data_standardized.txt")[,c(2:8)]
# p.vals = read.table("matrix_v.txt")

#i = 1
#plot(density(y[,i]), col = "red3")
#lines(seq(min(y[,i]), max(y[,i]), length.out = 100), dnorm(seq(min(y[,i]), max(y[,i]), length.out = 100), mean = mean(y[,i]), sd = sd(y[,i])), col = "black")

my.bkde2D = function(x, y){	
	if(min(sd(x), sd(y)) == 0){
		plot(x, y)	
	}else{
		d = bkde2D(cbind(x, y), bandwidth = 1.06 * c(sd(x), sd(y))* length(x)^(-1/5))
		contour(d$x1, d$x2, d$fhat)
	}
}
pdf("PlotsAgainstNorm.pdf", width = 16, height = 16)

m.layout = matrix(dim(p.vals)[2] * (dim(p.vals)[2]  - 1) / 2+dim(p.vals)[2] + 1, ncol = dim(p.vals)[2], nrow = dim(p.vals)[2])
m.layout[lower.tri(m.layout)] = 1:(dim(p.vals)[2] * (dim(p.vals)[2]  - 1) / 2)
m.layout = t(m.layout)
diag(m.layout) = 1:dim(p.vals)[2] + max(m.layout[upper.tri(m.layout)])
m.layout[lower.tri(m.layout)] = max(diag(m.layout)+1):(dim(p.vals)[2]^2)

layout(m.layout)
par(mai = c(0.1,0.1,0.1,0.1))
for(i in 1:(dim(p.vals)[2]-1))
for(j in (i+1):dim(p.vals)[2])
my.bkde2D(p.vals[,i], p.vals[,j])

for(i in 1:dim(p.vals)[2]){
	plot(density(p.vals[,i]), col = "red3", main = "")
	lines(seq(min(p.vals[,i]), max(p.vals[,i]), length.out = 100), dnorm(seq(min(p.vals[,i]), max(p.vals[,i]), length.out = 100), mean = mean(p.vals[,i]), sd = sd(p.vals[,i])), col = "black")
}
	
for(i in 1:(dim(p.vals)[2]-1))
for(j in (i+1):dim(p.vals)[2])
my.bkde2D(qnorm(dim(p.vals)[1] * ecdf(p.vals[,i])(p.vals[,i]) / (dim(p.vals)[1]+1)), qnorm(dim(p.vals)[1] * ecdf(p.vals[,j])(p.vals[,j]) / (dim(p.vals)[1]+1)))
dev.off()
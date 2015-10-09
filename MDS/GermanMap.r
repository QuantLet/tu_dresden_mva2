rm(list=ls(all=TRUE))
graphics.off()
setwd("/Users/ookhrin/Documents/Private/Lehre/MVA2/_excersises/MDS/")

# install.packages("MASS")
library(MASS)                               # load the MASS library
air = read.table("Distance_Air.csv", sep = ",")
city.names = as.character(as.matrix(air[1,-1]))
air = as.matrix(air[-1,-1])
air = matrix(as.integer(air), ncol = dim(air)[1], nrow = dim(air)[1])
colnames(air) = rownames(air) = city.names
air = t(air)
dist.air = as.dist(air)
iso.air = isoMDS(dist.air)

plot(iso.air$points, type = "n")
text(iso.air$points, city.names)

# rotate the map
angle = pi/2
rotation.matrix = cbind(c(cos(angle), -sin(angle)), c(sin(angle), cos(angle)))
new.coord = iso.air$points %*% rotation.matrix

layout(t(c(1, 2)))
plot(new.coord, type = "p", pch = 19, ylim = range(new.coord[,2]) * 1.3, xlim = range(new.coord[,1]) * 1.3)
text(new.coord[,1], new.coord[,2] - 30, city.names)

#### same for road distances 

road = read.table("Distance_Road.csv", sep = ",")
city.names = as.character(as.matrix(road[1,-1]))
road = as.matrix(road[-1,-1])
road = matrix(as.integer(road), ncol = dim(road)[1], nrow = dim(road)[1])
colnames(road) = rownames(road) = city.names
road = t(road)
dist.road = as.dist(road)
iso.road = isoMDS(dist.road)

# rotate the map
angle = pi/2
rotation.matrix = cbind(c(cos(angle), -sin(angle)), c(sin(angle), cos(angle)))
new.coord = iso.road$points %*% rotation.matrix

plot(new.coord, type = "p", pch = 19, ylim = range(new.coord[,2]) * 1.3, xlim = range(new.coord[,1]) * 1.3)
text(new.coord[,1], new.coord[,2] - 30, city.names)

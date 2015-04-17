## Demo showing how `DendSer::dser' can be used to optimize dendrogram
## ordering in `idendro'.
## (This demo is based on the example provided for `dser'.)
##

library(DendSer) # dser
library(idendro) # idendro

# hierarchical cluster analysis over iris data
d <- dist(scale(iris[, -5]))
h <- hclust(d)

# compute the first principal component (PC1) of iris data
PC1 <- prcomp(iris[,-5], scale = TRUE)$x[, 1]
# for visualization purposes, and PC1 to the data
iris.with.pc1 <- cbind(iris, PC1)

# draw dendrogram with heat map
idendro(h, iris.with.pc1, geometry=c(0,0,600,400))
# note the order of the observations (rows in heat map) does not
# reflect the Species well (and PC1 either)

# Let's reorder the observations by the PC1 using 'DendSer::dser'
h$order <- dser(h, PC1, cost = costLS)
idendro(h, iris.with.pc1, geometry=c(600,0,600,400))
# note the order of observations is much more natural when sorted

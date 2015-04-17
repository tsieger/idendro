## Simple dendrogram demo.
##

library(idendro) # idendro

data(iris)

# perform hierarchical clustering
hx<-hclust(dist(iris[, 1:4]))

# visualize clusters
idendro(hx)

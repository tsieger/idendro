## Simple dendrogram demo.
##

data(iris)

# perform hierarchical clustering
hx<-hclust(dist(iris[, 1:4]))

# visualize clusters
idendro(hx)

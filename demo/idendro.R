## Dendrogram + heat map demo.
##

data(iris)

# perform hierarchical clustering analysis
hx<-hclust(dist(iris[, 1:4]))

# visualize clusters and heat map
idendro(hx, iris)

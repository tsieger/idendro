## Dendrogram + heat map demo.
##

library(idendro) # idendro

data(iris)

# perform hierarchical clustering analysis
hx<-hclust(dist(iris[, 1:4]))

# visualize clusters and heat map
idendro(hx, iris)

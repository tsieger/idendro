## Demo of dendrogram + heat map plots with cranvas scatter plot
## integrated.
##

require(cranvas)

data(iris)

# compute pairwise distances
dx<-dist(iris[, 1:4])

# perform hierarchical clustering
hx<-hclust(dx)

# prepare mutable data frame enabling bidirectional communication
# between qscatter (coloring observations according to clusters
# currently selected by idendro) and idendro (displaying brushed
# observations)
qx<-qdata(iris)

# visualize clusters
idendro(hx,qx,maxClusterCount=6)

# visualize data on a scatter plot
print(qscatter(Sepal.Length,Sepal.Width,data=qx))

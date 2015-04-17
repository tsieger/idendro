## Demo of dendrogram + heat map plots with cranvas scatter plot
## integrated.
##

library(idendro) # idendro
library(cranvas) # qdata, qscatter, qparallel

data(iris)

# compute pairwise distances
dx <- dist(iris[, 1:4])

# perform hierarchical clustering
hx <- hclust(dx)

# prepare mutable data frame enabling bidirectional communication
# between qscatter (coloring observations according to clusters
# currently selected by idendro) and idendro (displaying brushed
# observations)
qx <- qdata(iris)

# visualize clusters
idendro(hx, qx)

# visualize data on a scatter plot
print(qscatter(Sepal.Length, Sepal.Width, data = qx))
# and a parallel coordinate plot
print(qparallel(~., data = qx))

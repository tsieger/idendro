## Demo of dendrogram + heat map integrated with two more feature space
## projections: a scatter plot depicting the first two principal
## components of the data, and a "tour" projection (via the `tourr'
## package).
##

library(tourr) # animate_xy

x<-flea[,1:6]
# compute pairwise distances
dx<-dist(x)

# perform hierarchical clustering
hx<-hclust(dx)

# visualize clusters
qx<-idendro(hx,x,maxClusterCount=6,doSmoothHeatmap=T)

# compute PCA ...
x.pc <- prcomp(x,center=TRUE,scale.=TRUE)
qx$PC1 <- x.pc$x[, 1]
qx$PC2 <- x.pc$x[, 2]
# ... and show the first two principal components
qscatter(PC1, PC2, qx)

# now select some clusters in the dendrogram
# once you are done, run this code to visualize data on a tour plot:
#
#  animate_xy(x,col=qx$.color)

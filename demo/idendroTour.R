## Demo of dendrogram + heat map integrated with "tour" feature-space
## projection of the data (via the `tourr' package).
##

require(tourr) # animate_xy

x<-flea[,1:6]
# compute pairwise distances
dx<-dist(x)

# perform hierarchical clustering
hx<-hclust(dx)

# visualize clusters
qx<-idendro(hx,x,maxClusterCount=6,doSmoothHeatmap=T)
# now select some clusters in the dendrogram

# once you are done, run this code to visualize data on a tour plot:
#animate_xy(x,col=qx$.color)


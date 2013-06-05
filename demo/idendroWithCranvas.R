## Demo of dendrogram + heat map plots with cranvas scatter plot
## integrated.
##

require(cranvas)

# generate data in feature space
n<-10
x<-data.frame(x1=c(rnorm(n,-3),rnorm(n,3)),x2=c(rnorm(n,-3),rnorm(n,3)))
rownames(x)<-1:(2*n)

# compute pairwise distances
dx<-dist(x)

# perform hierarchical clustering
hx<-hclust(dx)

# prepare mutable data frame enabling bidirectional communication
# between qscatter (coloring observations according to clusters
# currently selected by idendro) and idendro (displaying brushed
# observations)
qx<-qdata(x)

# visualize clusters
idendro(hx,qx,maxClusterCount=6)

# visualize data on a scatter plot
print(qscatter(x1,x2,data=qx))

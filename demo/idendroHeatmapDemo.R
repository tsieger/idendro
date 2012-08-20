## Simple demo showing how `idendro' visualizes a dendrogram enriched
## with a heatmap depicting feature space of observation.
##

# generate data (feature space)
n<-10
x<-data.frame(x1=c(rnorm(n,-1),rnorm(n,1)),x2=c(rnorm(n,-1),rnorm(n,1)))
rownames(x)<-1:(2*n)

# compute pairwise distances
dx<-dist(x)

# perform hierarchical clustering
hx<-hclust(dx)

# visualize clusters, the heatmap is enabled by default
idendro(hx,x)


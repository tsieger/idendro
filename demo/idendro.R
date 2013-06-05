## Dendrogram + heat map demo.
##

# generate data in feature space
n<-10
x<-data.frame(x1=c(rnorm(n,-3),rnorm(n,3)),x2=c(rnorm(n,-3),rnorm(n,3)))
rownames(x)<-1:(2*n)

# perform hierarchical clustering analysis
hx<-hclust(dist(x))

# visualize clusters
idendro(hx,x)

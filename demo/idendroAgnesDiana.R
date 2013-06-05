## Simple visualization of the result of `cluster::agnes' and
## `cluster::diana'.
##

require(cluster)

# generate data in feature space
n<-10
x<-data.frame(x1=c(rnorm(n,-2),rnorm(n,2)),x2=c(rnorm(n,-2),rnorm(n,2)))
rownames(x)<-1:(2*n)

# perform hierarchical clustering using `agnes'
hx.agnes<-agnes(x)
# visualize clusters
idendro(hx.agnes,x)

# perform hierarchical clustering using `diana'
hx.diana<-diana(x)
# visualize clusters
idendro(hx.diana,x)

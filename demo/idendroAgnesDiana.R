## Simple visualization of the result of `cluster::agnes' and
## `cluster::diana'.
##

require(cluster)


data(iris)

dx<-dist(iris[, 1:4])

# perform hierarchical clustering using `agnes'
hx.agnes<-agnes(dx)
# visualize clusters
idendro(hx.agnes,iris)

# perform hierarchical clustering using `diana'
hx.diana<-diana(dx)
# visualize clusters
idendro(hx.diana,iris)

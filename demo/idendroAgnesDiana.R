## Simple visualization of the result of `cluster::agnes' and
## `cluster::diana'.
##

library(cluster)
library(idendro) # idendro

data(iris)

dx <- dist(iris[, 1:4])

# perform hierarchical clustering using `agnes'
hx.agnes <- agnes(dx)
# visualize clusters
idendro(hx.agnes, iris, geometry=c(0,0,600,400))

# perform hierarchical clustering using `diana'
hx.diana <- diana(dx)
# visualize clusters
idendro(hx.diana, iris, geometry=c(600,0,600,400))

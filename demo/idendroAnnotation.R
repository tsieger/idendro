## Simple demo showing how `idendro' visualizes long dimnames and case
## names in the heat map.
##

library(idendro) # idendro

# generate data in feature space
n <- 10
x <- data.frame(very.long.dim.name1 = c(rnorm(n, -1), rnorm(n, 1)),
    long.dim.name2 = c(rnorm(n, -1), rnorm(n, 1)))
rownames(x) <- paste('my brand new observation', 1:(2*n))

# compute pairwise distances
dx <- dist(x)

# perform hierarchical clustering
hx <- hclust(dx)

# visualize clusters
idendro(hx, x)

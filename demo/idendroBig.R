## Demo showing `idendro' bidirectional integration with
## `cranvas::qscatter' plot using "big data" (5000 observations).
##

require(cranvas)

# load "big data"
data(hca5000)

# prepare mutable data frame enabling bidirectional communication
# between qscatter (coloring observations according to clusters
# currently selected by idendro) and idendro (displaying brushed
# observations)
qx<-qdata(hca5000$x)

# visualize clusters
idendro(hca5000$hx,qx,maxClusterCount=6)

# visualize data on a scatter plot
print(qscatter(x1,x2,data=qx,unibrushcolor=FALSE))

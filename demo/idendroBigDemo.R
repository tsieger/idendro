## Demo showing `idendro' performance on big data.
##

# load "big data"
data(hca5000)

# visualize clusters
idendro(hca5000$hx,hca5000$x,brushedmapEnabled=FALSE,observationAnnotationEnabled=FALSE,heatmapEnabled=TRUE)

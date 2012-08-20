## Demo showing `idendro' performance on big data.
##

# load "big data"
data(hca5000)

colnames(x)<-1:ncol(x)
rownames(x)<-1:nrow(x)

# visualize clusters
idendro(hx,x,brushedmapEnabled=FALSE,observationAnnotationEnabled=FALSE,heatmapEnabled=TRUE)

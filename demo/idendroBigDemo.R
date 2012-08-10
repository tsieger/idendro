## Demo showing `idendro' performance on big data.
##

# load "big data"
data(hca5000)

    # hack when run not from an installed package
    if (!exists('hx')) load('~/src/R/idendro/idendro/data/hca5000.rda')

colnames(x)<-1:ncol(x)
rownames(x)<-1:nrow(x)

# visualize clusters
idendro(hx,x,brushedmapEnabled=FALSE,observationAnnotationEnabled=FALSE,heatmapEnabled=TRUE)

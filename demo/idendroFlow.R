## Visualization of flow cytometry data in dendrogram and heat
## map, with cranvas scatter plots integrated.
##

library(flowStats) # ITN
library(RColorBrewer) # brewer.pal
data(ITN)

# get data matrix
x <- exprs(ITN$sample03[, 1:7])
# preprocess the data
x[, 3:7] <- log10(x[, 3:7])
x <- scale(x)

# perform HCA
hx <- hclust(dist(x), method = 'average')

# plot dendrogram + heat map
mdf.x <- idendro(hx, x, 
    heatmapColors = colorRampPalette(c("purple4", "blue3", "blue3", "grey", 
        "grey", "orangered", "orangered", "red")(15)),
    clusterColors = brewer.pal(12, "Paired"))

# plot scatter plots of selected data projections
sp1 <- qscatter("CD3", "HLADr", mdf.x)
print(sp1)
sp2 <- qscatter("CD8", "CD4", mdf.x)
print(sp2)

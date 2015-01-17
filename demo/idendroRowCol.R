## Demo on clustering both rows and columns of a data set.
##

data(iris)

# Find similar variables (i.e. features measured on the Iris flowers:
# sepal/petal widths/lengths) ...
hx <- hclust(dist(t(scale(iris[, 1:4]))))
# ... and display the similarities in a dendrogram.
idendro(hx, t(iris[, 1:4]), heatmapRelSize=.7, doScaleHeatmapByRows=TRUE, geometry=c(0,0,600,400))
# (Petal width and length being most similar.)

# Then display clusters of similar Iris flowers in another
# dendrogram. As we sort variables, similar variables appear in columns
# close to each other.
hx2 <- hclust(dist(iris[, 1:4]))
idendro(hx2, iris[,c(rev(hx$order),5)], geometry=c(600,0,600,400))

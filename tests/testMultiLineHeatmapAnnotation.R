## Test multi-line hetamap annotation.
##

if (interactive()) {
    data(iris)

    hx<-hclust(dist(iris[, 1:4]))
    levels(iris$Species)[1]<-'1\n22222\n333'
    cat('Click in the upper right corner of the heatmap - is the annotation dispalyed well?\n')
    idendro(hx, iris)
}

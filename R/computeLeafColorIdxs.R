# Compute colors of all leafs in dendrogram.
computeLeafColorIdxs <- function(df) {
    leafColorIdxs<-rep(0,df$n)
    for (i in seq(along=df$clusters)) {
        if (!is.null(df$clusters[[i]]) && length(df$clusters[[i]]$indices)>0) {
            members<-computeMemberIndices(df$h,max(df$clusters[[i]]$indices))
            leafColorIdxs[members]<-i
        }
    }
    return(leafColorIdxs)
}

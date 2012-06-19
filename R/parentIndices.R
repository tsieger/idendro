parentIndices<-function(h,clusterIdx) {
    clusterCount<-nrow(h$merge)
    indices<-rep(NA,clusterCount)
    indicesCount<-0
    repeat {
        parent<-parents[clusterIdx]
        if (is.na(parent)) break
        indicesCount<-indicesCount+1
        indices[indicesCount]<-clusterIdx<-parent
    }
    return(indices[1:indicesCount])
}

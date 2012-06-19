subclusterIndices<-function(h,clusterIdx) {
    clusterCount<-nrow(h$merge)
    indices<-rep(NA,clusterCount)
    indicesCount<-0
    toTraverse<-c(clusterIdx,rep(NA,clusterCount-1))
    toTraverseStart<-1
    toTraverseEnd<-2
    repeat {
        i<-toTraverse[toTraverseStart]
        if (is.na(i)) break
        toTraverse[toTraverseStart]<-NA
        toTraverseStart<-toTraverseStart+1
        indicesCount<-indicesCount+1
        indices[indicesCount]<-i
        if (h$merge[i,1]>0) {
            toTraverse[toTraverseEnd]<-h$merge[i,1]
            toTraverseEnd<-toTraverseEnd+1
        }
        if (h$merge[i,2]>0) {
            toTraverse[toTraverseEnd]<-h$merge[i,2]
            toTraverseEnd<-toTraverseEnd+1
        }
    }
    return(indices[1:indicesCount])
}

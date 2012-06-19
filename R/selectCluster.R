selectCluster<-function (pos) {
    dbg<-gfc(dbg)
    dbg.clusterSelector<-gfc(dbg.clusterSelector)
    if (dbg.clusterSelector) print('selectCluster called')

    df<-gfc(df)

    if (dbg.clusterSelector) print(pos)
    x<-as.numeric(pos)[1]
    y<-as.numeric(pos)[2]
    if (dbg.clusterSelector) printVar(x)
    if (dbg.clusterSelector) printVar(y)
    gw<-xy2gw(list(x,y))
    if (dbg.clusterSelector) printVar(gw)
    #TODO:
    #currentXlim<-c(0,1)
    #currentYlim<-c(0,1)
    #d<-((-df$h$height-x)/diff(currentXlim))^2+((df$branchCenterOffsets-y)/diff(currentYlim))^2
    d<-(dendro2fig(list(df$h$height,NULL))[[1]]-gw[[1]])^2+(df$branchCenterOffsets-gw[[2]])^2
    if (dbg.clusterSelector) printVar(d)
    minI<-which.min(d)
    if (dbg.clusterSelector) printVar(minI)
    subclusters<-subclusterIndices(df$h,minI)
    if (dbg.clusterSelector) printWithName(subclusters)
    idx<-clusterId2SegmentIds(subclusters)
    df$clusters[[df$currentCluster]] <-
        list(indices=subclusters,
            branches=with(df$allBranches$branches,data.frame(x1s=x1s[idx],x2s=x2s[idx],y1s=y1s[idx],y2s=y2s[idx])))
    unselectedIndices<-setdiff(1:df$clusterCount,subclusters)
    if (dbg.clusterSelector) printWithName(unselectedIndices)
    # remove the branches selected from the other clusters
    for (i in seq(along=df$clusters)) {
        if (i!=df$currentCluster && !is.null(df$clusters[[i]])) {
            if (length(intersect(subclusters,df$clusters[[i]]$indices))>0) {
                df$clusters[[i]]$indices<-NULL
                df$clusters[[i]]$branches<-NULL
            } else {
                unselectedIndices<-setdiff(unselectedIndices,df$clusters[[i]]$indices)
            }
        }
    }
    # remove branches of selected clusters from unselectedBranches
    if (dbg.clusterSelector) printWithName(unselectedIndices)
    df$unselectedBranches$indices<-unselectedIndices
    idx<-clusterId2SegmentIds(unselectedIndices)
    df$unselectedBranches$branches<-with(df$allBranches$branches,data.frame(x1s=x1s[idx],x2s=x2s[idx],y1s=y1s[idx],y2s=y2s[idx]))
    df
}

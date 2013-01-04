selectCluster<-function
### Select the nearest cluster to the position 'pos'.
##keyword<<internal
(
    pos ##<< position in the dendro figure
) {
    dbg<-.gfc(dbg)
    dbg.dendro.select<-.gfc(dbg.dendro.select)
    if (dbg.dendro.select) print('selectCluster called')

    df<-.gfc(df)

    # remember the current selection
    df<-pushSelectionHistory(df)
    df$lastSelectionSaver<-'selectCluster'

    if (dbg.dendro.select) print(pos)
    x<-as.numeric(pos)[1]
    y<-as.numeric(pos)[2]
    if (dbg.dendro.select) printVar(x)
    if (dbg.dendro.select) printVar(y)
    gw<-xy2gw(list(x,y))
    if (dbg.dendro.select) printVar(gw)

    ## find nearest cluster merging points
    # zoom region determines scaling
    dendroZoom<-.gfc(dendroZoom)
    zoom.limits.gw<-dendro2fig(dendroZoom)
    if (dbg.dendro.select) printVar(zoom.limits.gw)
    # branching points
    if (dbg.dendro.select) printVar(dendro2fig(list(df$h$height,NULL)))
    branches.g<-dendro2fig(list(df$h$height[df$clusterCount]-df$h$height,NULL))$g
    if (dbg.dendro.select) printVar(branches.g)
    branches.w<-df$branchCenterOffsets
    if (dbg.dendro.select) printVar(branches.w)
    # distances (measured in the current zoom region)
    d<-((branches.g-gw$g)/diff(zoom.limits.gw$g))^2+((branches.w-gw$w)/diff(zoom.limits.gw$w))^2
    if (dbg.dendro.select) printVar(d)
    # index of the nearest cluster
    minI<-which.min(d)
    if (dbg.dendro.select) printVar(minI)

    # subclusters
    subclusters<-computeSubclusterIndices(df$h,minI)
    if (dbg.dendro.select) printWithName(subclusters)
    idx<-clusterId2SegmentIds(subclusters)

    # mark selected dendrogram segments
    df$clusters[[df$currentCluster]] <-
        list(indices=subclusters,
            branches=with(df$allBranches$branches,data.frame(x1s=x1s[idx],x2s=x2s[idx],y1s=y1s[idx],y2s=y2s[idx])))

    # unmark deselected subclusters
    unselectedIndices<-setdiff(1:df$clusterCount,subclusters)
    if (dbg.dendro.select) printWithName(unselectedIndices)

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
    if (dbg.dendro.select) printWithName(unselectedIndices)
    df$unselectedBranches$indices<-unselectedIndices
    idx<-clusterId2SegmentIds(unselectedIndices)
    df$unselectedBranches$branches<-with(df$allBranches$branches,data.frame(x1s=x1s[idx],x2s=x2s[idx],y1s=y1s[idx],y2s=y2s[idx]))

    # compute leaf colors
    df$leafColorIdxs<-computeLeafColorIdxs(df)

    if (dbg.dendro.select>1) printWithName(df)
    return(df)
    ### shared data frame
}

prepareDendro<-function
### Perform all computations needed to display a dendrogram and
### initialize a shared data frame.
###
### This is an internal function not to be called by the user.
##keyword<<internal
(
    h, ##<< an object of class 'stats::hclust' describing a clustering

    x = NULL, ##<< a data frame holding observations tha were clustered
    ## giving rise to 'h'

    dbg = 0 ##<< debug verbosity level
) {

    df<-NULL
    df$h<-h
    df$x<-x
    df$clusterCount<-clusterCount<-length(h$height)
    df$n<-n<-df$clusterCount+1
    if (is.null(x)) {
        df$k<-0
    } else {
        df$k<-ncol(x)
    }

    if (dbg) printVar(n)
    if (dbg) printVar(clusterCount)

    if (dbg) cat('Computing memberCounts...\n')
    memberCounts<-matrix(NA,clusterCount,2)
    for (i in 1:clusterCount) {
        memberCounts[i,1]<-ifelse(h$merge[i,1]<0,1,sum(memberCounts[h$merge[i,1],]))
        memberCounts[i,2]<-ifelse(h$merge[i,2]<0,1,sum(memberCounts[h$merge[i,2],]))
    }
    if (dbg>1) printVar(memberCounts)

    if (dbg) cat('Computing parents...\n')
    parents<-rep(NA,clusterCount)
    # indices of non-trivial subclusters (having more than one meber):
    idx<-h$merge>0
    # h$merge consists of two columns representing merged subclusters.
    # The id's of such subclusters are 1..clusterCount in each column,
    # thus the id's are in the form of
    #   cbind(1..clusterCount,1..clusterCount)
    # which is equal to
    #   rep(1:clusterCount,2)
    # Parents of subcluster h$merge[i,j] is 'i', thus parents of
    # h$merge are rep(1:clusterCount,2) and parents of non-trivial
    # subclusters are rep(1:clusterCount,2)[idx]:
    parents[h$merge[idx]]<-rep(1:clusterCount,2)[idx]
    if (dbg>1) printVar(parents)

    if (dbg) cat('Computing offsets...\n')
    clusterOffsets<-rep(NA,clusterCount)
    clusterOffsets[clusterCount]<-0
    leafOffsets<-rep(NA,n)
    for (i in clusterCount:1) {
        c1<-h$merge[i,1]
        # lower branch offset is dictated by the parent branch
        if (c1>0) {
            clusterOffsets[c1]<-clusterOffsets[i]
        } else {
            leafOffsets[n+1+c1]<-clusterOffsets[i]
        }
        c2<-h$merge[i,2]
        # upper branch offset is dictated by the sum of parent branch
        # offset and the width of the lower branch
        if (c2>0) {
            clusterOffsets[c2]<-clusterOffsets[i]+ifelse(c1>0,sum(memberCounts[c1,]),1)
        } else {
            leafOffsets[n+1+c2]<-clusterOffsets[i]+ifelse(c1>0,sum(memberCounts[c1,]),1)
        }
    }
    offsets<-c(leafOffsets,NA,clusterOffsets)
    if (dbg>1) printVar(clusterOffsets)
    if (dbg>1) printVar(leafOffsets)
    if (dbg>1) printVar(offsets)

    if (dbg) cat('Computing yPos...\n')
    yPos<-c(1+leafOffsets,NA,rep(NA,clusterCount))
    for (i in 1:clusterCount) {
        yPos[n+1+i]<-mean(yPos[n+1+h$merge[i,]])
    }
    if (dbg>1) printVar(yPos)

    leafOrder<-order(computeLeafOrder(h$merge))

    if (dbg) cat('Computing segments...\n')
    x1s<-x2s<-y1s<-y2s<-rep(NA,3*clusterCount)
    ii<-0
    branchCenterOffsets<-rep(NA,clusterCount)
    for (i in 1:clusterCount) {
        x0<-h$height[i]
        x1<-ifelse(h$merge[i,1]>0,h$height[h$merge[i,1]],0)
        x2<-ifelse(h$merge[i,2]>0,h$height[h$merge[i,2]],0)
        # mirror Xs: x=0 corresponding to the top-most cluster, x>0 to leafs
        x0<-h$height[clusterCount]-x0
        x1<-h$height[clusterCount]-x1
        x2<-h$height[clusterCount]-x2

        y1<-yPos[n+1+h$merge[i,1]]
        y2<-yPos[n+1+h$merge[i,2]]
        branchCenterOffsets[i]<-mean(c(y1,y2))
        # x1,y1 -> x0,y1
        # x0,y1 -> x0,y2
        # x2,y2 -> x0,y2
        x1s[ii+(1:3)]<-c(x1,x0,x2)
        x2s[ii+(1:3)]<-c(x0,x0,x0)
        y1s[ii+(1:3)]<-c(y1,y1,y2)
        y2s[ii+(1:3)]<-c(y1,y2,y2)
        ii<-ii+3
    }
    if (dbg>1) printVar(x1s)
    if (dbg>1) printVar(x2s)
    if (dbg>1) printVar(y1s)
    if (dbg>1) printVar(y2s)
    #coords1<-gw2xy(dendro2fig(with(df$unselectedBranches$branches,list(x1s,y1s))))
    #coords2<-gw2xy(dendro2fig(with(df$unselectedBranches$branches,list(x2s,y2s))))
    coords1<-gw2xy(dendro2fig(list(x1s,y1s)))
    coords2<-gw2xy(dendro2fig(list(x2s,y2s)))
    x1s<-coords1[[1]]
    y1s<-coords1[[2]]
    x2s<-coords2[[1]]
    y2s<-coords2[[2]]
    if (dbg>1) printVar(x1s)
    if (dbg>1) printVar(x2s)
    if (dbg>1) printVar(y1s)
    if (dbg>1) printVar(y2s)
    branchCenterOffsets<-dendro2fig(list(NULL,branchCenterOffsets))[[2]]

    if (dbg) cat('Initializing the shared data frame...\n')
    df$unselectedBranches<-df$allBranches<-list(indices=1:clusterCount,branches=data.frame(x1s=x1s,x2s=x2s,y1s=y1s,y2s=y2s))
    df$clusters<-NULL
    df$currentCluster<-1
    df$clusterCount<-clusterCount
    df$branchCenterOffsets<-branchCenterOffsets
    df$leafOrder<-leafOrder
    df$leafColorIdxs<-rep(0,n)
    df$xOrdered<-x[leafOrder,,drop=F]
    df$xOrderedSmoothed<-df$xOrdered
    df$elemClusterCount<-df$n

    return(df)
    ### shared data frame
}

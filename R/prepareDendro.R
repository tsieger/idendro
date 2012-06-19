prepareDendro<-function(h,x=NULL,dbg=0) {

    df<-NULL
    df$h<-h
    df$x<-x

    clusterCount<-nrow(h$merge)
    n<-clusterCount+1
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
    for (i in clusterCount:1) {
        #printWithName(i)
        #printWithName(pmax(0,h$merge[i,]))
        parents[pmax(0,h$merge[i,])]<-i
    }
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
    coords1<-gw2xy(dendro2fig(with(df$unselectedBranches$branches,list(x1s,y1s))))
    coords2<-gw2xy(dendro2fig(with(df$unselectedBranches$branches,list(x2s,y2s))))
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
    df$fetchedBranches<-df$noBranches<-list(indices=c(),branches=data.frame(x1s=c(),x2s=c(),y1s=c(),y2s=c()))
    df$fetchedLeafCount<-0
    df$fetchedInfo<-NULL
    if (dbg) printVar(df$fetchedMap)
    fetchedMap<-matrix(rep(0,n),nrow=1)
    df$fetchedMap<-matrix(rep(0,n),nrow=1)
    if (dbg) printVar(df$fetchedMap)
    df$emptyFetchedMap<-matrix(rep(0,n),nrow=1)
    df$fetchedChanged<-FALSE
    df$currentCluster<-1
    df$clusterCount<-clusterCount
    df$branchCenterOffsets<-branchCenterOffsets

    df
}

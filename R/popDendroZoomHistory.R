popDendroZoomHistory <- function
### Restore (and discard from history) the last dendro zoom.
##keyword<<internal
(
    .sharedEnv, ##<< shared environment (the environment of the
    ## 'idendro' function)
    dbg=FALSE ##<< debug flag/level
) {
    if (dbg) cat('popDendroZoomHistory called\n')

    if (length(.sharedEnv$df$dendroZoomHistory)>0) {
        dendroZoom<-.sharedEnv$df$dendroZoomHistory[[1]]
        .sharedEnv$df$dendroZoomHistory<-.sharedEnv$df$dendroZoomHistory[-1]
    } else {
        dendroZoom<-NULL
    }
    if (dbg) printVar(dendroZoom)
    if (dbg) printVar(length(.sharedEnv$df$dendroZoomHistory))
    return(dendroZoom)
    ### dendro zoom
}

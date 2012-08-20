popDendroZoomHistory <- function
### Restore (and discard from history) the last dendro zoom.
(
    .sharedEnv ##<< shared environment (the environment of the
    ## 'idendro' function)
) {
    dbg.dendro.zoom<-gfc(dbg.dendro.zoom)

    if (dbg.dendro.zoom) cat('popDendroZoomHistory called\n')

    if (length(.sharedEnv$df$dendroZoomHistory)>0) {
        dendroZoom<-.sharedEnv$df$dendroZoomHistory[[1]]
        .sharedEnv$df$dendroZoomHistory<-.sharedEnv$df$dendroZoomHistory[-1]
    } else {
        dendroZoom<-NULL
    }
    if (dbg.dendro.zoom) printVar(dendroZoom)
    if (dbg.dendro.zoom) printVar(length(.sharedEnv$df$dendroZoomHistory))
    return(dendroZoom)
    ### dendro zoom
}

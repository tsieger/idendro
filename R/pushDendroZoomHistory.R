# Save the current dendroZoom (as stored in .sharedEnv).
pushDendroZoomHistory <- function(.sharedEnv) {
    dbg.dendro.zoom<-gfc(dbg.dendro.zoom)

    if (dbg.dendro.zoom) cat('pushDendroZoomHistory called\n')
    if (dbg.dendro.zoom) printVar(.sharedEnv$dendroZoom)

    .sharedEnv$df$dendroZoomHistory<-c(list(.sharedEnv$dendroZoom),.sharedEnv$df$dendroZoomHistory)

    if (dbg.dendro.zoom) printVar(length(.sharedEnv$df$dendroZoomHistory))
}

pushDendroZoomHistory <- function
### Save the current dendroZoom (as stored in '.sharedEnv').
##keyword<<internal
(
    .sharedEnv, ##<< shared environment (the environment of the
    ## 'idendro' function)
    dbg=FALSE ##<< debug flag/level
) {
    if (dbg) cat('pushDendroZoomHistory called\n')
    if (dbg) printVar(.sharedEnv$dendroZoom)

    .sharedEnv$df$dendroZoomHistory<-c(list(.sharedEnv$dendroZoom),.sharedEnv$df$dendroZoomHistory)

    if (dbg) printVar(length(.sharedEnv$df$dendroZoomHistory))
}

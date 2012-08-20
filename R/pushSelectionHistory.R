pushSelectionHistory <- function
### Save the current cluster selection (as stored in df).
(
    df ##<< shared data frame
) {
    dbg.dendro.select<-gfc(dbg.dendro.select)

    if (dbg.dendro.select) cat('pushSelectionHistory called\n')
    if (dbg.dendro.select) printVar(length(df$clusters))
    if (dbg.dendro.select>1) printVar(df$clusters)

    df$selectionHistory<-c(
        list(list(clusters=df$clusters,
            leafColorIdxs=df$leafColorIdxs,
            unselectedBranches=df$unselectedBranches)),
        df$selectionHistory)

    if (dbg.dendro.select) printVar(length(df$selectionHistory))

    return(df)
    ### shared data frame with cluster selection pushed on top of the
    ### selection stack.
}

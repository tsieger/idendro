# Restore (and discard from history) the last cluster selection.
popSelectionHistory <- function(df) {
    dbg.dendro.select<-gfc(dbg.dendro.select)
    if (dbg.dendro.select) cat('popSelectionHistory called\n')

    if (length(df$selectionHistory)>0) {
        selection<-df$selectionHistory[[1]]
        df$selectionHistory<-df$selectionHistory[-1]
        df$clusters<-selection$clusters
        df$leafColorIdxs<-selection$leafColorIdxs
        df$unselectedBranches<-selection$unselectedBranches
    } else {
        selection<-NULL
    }

    if (dbg.dendro.select) printVar(length(selection$clusters))
    if (dbg.dendro.select>1) printVar(selection)
    if (dbg.dendro.select) printVar(length(df$selectionHistory))

    if (is.null(selection)) {
        return(NULL)
    } else {
        return(df)
    }
}

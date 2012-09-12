unselectAllClusters<-function
### Unselect all clusters.
##keyword<<internal
(
    df ##<< shared data frame
) {
    dbg<-.gfc(dbg)
    dbg.dendro.select<-.gfc(dbg.dendro.select)

    if (dbg.dendro.select) cat('unselectAllClusters called\n')

    if (df$lastSelectionSaver!='unselectAll') {
        # remember current selection
        df<-pushSelectionHistory(df)
        df$lastSelectionSaver<-'unselectAll'
    }

    if (!is.null(df$clusters)) {
        df$clusters<-NULL
        df$leafColorIdxs<-0
        df$unselectedBranches<-df$allBranches
        selectionChanged<-TRUE
    } else {
        # selection has not changed
        selectionChanged<-FALSE
    }
    return(list(df=df,selectionChanged=selectionChanged))
    ### a list of a shared data frame 'df' and a boolean flag
    ### 'selectionChanged' determing if cluster selection has changed
    ### (so the caller can learn whether to redraw clusters).
}

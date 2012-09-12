unselectCurrentCluster<-function
### Unselect the current cluster.
##keyword<<internal
(
    df ##<< shared data frame
) {
    dbg.dendro.select<-.gfc(dbg.dendro.select)

    if (dbg.dendro.select) cat('unselectCluster called\n')
    if (dbg.dendro.select) printVar(df$currentCluster)

    # remember current selection
    df<-pushSelectionHistory(df)
    df$lastSelectionSaver<-'unselectCurrectCluster'


    if (!is.null(df$clusters[[df$currentCluster]])) {
        df$leafColorIdxs[df$clusters[[df$currentCluster]]$indices]<-0
        df$unselectedBranches$indices<-
            c(df$unselectedBranches$indices,df$clusters[[df$currentCluster]]$indices)
        df$unselectedBranches$branches<-
            rbind(df$unselectedBranches$branches,df$clusters[[df$currentCluster]]$branches)
        df$clusters[[df$currentCluster]]$indices<-NULL
        df$clusters[[df$currentCluster]]$branches<-NULL
        selectionChanged<-TRUE
    } else {
        # the current cluster was selected, nothing to do
        selectionChanged<-FALSE
    }
    return(list(df=df,selectionChanged=selectionChanged))
    ### a list of shared data frame 'df' and a boolean flag
    ### 'selectionChanged' determing if clsuter selection has changed
    ### (so the caller can learn whether to redraw clusters).
}

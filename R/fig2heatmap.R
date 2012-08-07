# figure -> heatmap coord conversion
fig2heatmap<-function(gw) {
    dbg.tx<-gfc(dbg.tx)
    if (dbg.tx) print('fig2heatmap called')

    return(gw)

    df<-gfc(df)

    n<-nrow(df$x)
    k<-ncol(df$x)
    g<-gw[[1]]
    w<-gw[[2]]
    list( g = k * (g - dendroG - selectorG) / heatmapG,
         w = n * (w - charmW) / (1-strangeW-charmW) + .5)
}

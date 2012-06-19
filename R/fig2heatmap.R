# figure -> heatmap coord conversion
fig2heatmap<-function(gw) {
    dbg.tx<-gfc(dbg.tx)
    if (dbg.tx) print('fig2heatmap called')

    df<-gfc(df)

    selectorG<-heatmapG/ncol(df$x)
    n<-nrow(df$x)
    k<-ncol(df$x)
    g<-gw[[1]]
    w<-gw[[2]]
    list( k * (g - dendroG - selectorG) / heatmapG,
         n * (w - charmW) / (1-strangeW-charmW) + .5)
}

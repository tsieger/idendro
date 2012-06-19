# heatmap -> figure coord conversion
heatmap2fig<-function(gw) {
    dbg.tx<-gfc(dbg.tx)
    if (dbg.tx) print('heatmap2fig called')

    df<-gfc(df)

    heatmapG<-gfc(heatmapG)
    charmW<-gfc(charmW)
    strangeW<-gfc(strangeW)
    dendroG<-gfc(dendroG)

    selectorG<-heatmapG/ncol(df$x)
    n<-nrow(df$x)
    k<-ncol(df$x)
    g<-gw[[1]]
    w<-gw[[2]]
    list(dendroG + selectorG + heatmapG * (g/k),
        (1-strangeW-charmW) * ((w-.5)/n) + charmW)
}

# heatmap -> figure coord conversion
heatmap2fig<-function(gw) {
    dbg.tx<-gfc(dbg.tx)
    if (dbg.tx) print('heatmap2fig called')

    return(gw)

    df<-gfc(df)

    charmW<-gfc(charmW)
    strangeW<-gfc(strangeW)
    dendroG<-gfc(dendroG)
    selectorG<-gfc(selectorG)
    heatmapG<-gfc(heatmapG)

    n<-nrow(df$x)
    k<-ncol(df$x)
    g<-gw[[1]]
    w<-gw[[2]]
    list(g=dendroG + selectorG + heatmapG * (g/k),
        w=(1-strangeW-charmW) * ((w)/n) + charmW)
}

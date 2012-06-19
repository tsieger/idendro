# dendrogram -> figure coords conversion
dendro2fig<-function(gw) {
    dbg.tx<-gfc(dbg.tx)
    if (dbg.tx) print('dendro2fig called')

    df<-gfc(df)
    charmW<-gfc(charmW)
    dendroG<-gfc(dendroG)
    strangeW<-gfc(strangeW)

    n<-nrow(df$x)
    k<-ncol(df$x)
    g<-gw[[1]]
    w<-gw[[2]]
    list(dendroG*(1-g/last(df$h$height)),
        (1-strangeW-charmW) * (1-(w-1)/(n-1))+charmW)
}

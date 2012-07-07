# fig -> dendro coord conversion
fig2dendro<-function(gw) {
    dbg.tx<-gfc(dbg.tx)
    if (dbg.tx) print('fig2dendro called')

    return(gw)

    df<-gfc(df)
    n<-nrow(df$x)
    k<-ncol(df$x)
    g<-gw[[1]]
    w<-gw[[2]]
    list(last(df$h$height) * (1 - g / gfc(dendroG) ),
        (n-1) * (1 - (w - gfc(charmW)) / (1-gfc(strangeW)-gfc(charmW)) ) + 1)
}

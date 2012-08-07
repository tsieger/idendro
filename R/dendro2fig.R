# dendrogram -> figure coords conversion
#
# g=gw[1]: dendro height ... 0
# w=gw[2]: n ... 1 (charm to strange)
dendro2fig<-function(gw) {
    dbg.tx<-gfc(dbg.tx)
    if (dbg.tx) print('dendro2fig called')

    names(gw)<-c('g','w')
    return(gw)

    df<-gfc(df)
    charmW<-gfc(charmW)
    dendroG<-gfc(dendroG)
    dendroZoom<-gfc(dendroZoom)
#printVar(dendroZoom)
    strangeW<-gfc(strangeW)

    n<-nrow(df$x)
    k<-ncol(df$x)
    g<-gw[[1]]
    w<-gw[[2]]

    # g=gw[1]: dendro height ... 0
    # w=gw[2]: n ... 1 (charm to strange)

    # zoom
    # dendroZoomG: vector of zoom limits h1,h2, h1>h2
    # dendroZoomW: vector of zoom limits w1,w2, w1>w2
    #g<-(g-dendroZoomG[2])/diff(dendroZoomG)
    #w<-(w-dendroZoomW[2])/diff(dendroZoomW)
    #g<-(g-mean(dendroZoom$g))*(-diff(dendroZoom$g))/last(df$h$height)+mean(dendroZoom$g)
    #w<-(w-mean(dendroZoom$w))*(-diff(dendroZoom$w))/n+mean(dendroZoom$w)

    # transform to layer coords
    list(g=dendroG*(1-g/last(df$h$height)),
        w=(1-strangeW-charmW) * (1/(2*n) + (n-w)/n)+charmW)
}

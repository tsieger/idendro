# physical x/y to logical grow/width coord conversion
xy2gw<-function(xy) {
    gw<-xy
    names(gw)<-c('g','w')
    gw
}

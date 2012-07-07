# logical grow/width to physical x/y coord conversion
gw2xy<-function(gw) {
    xy<-gw
    names(xy)<-c('x','y')
    xy
}

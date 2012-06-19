# Compute cardinalities of cluters in hca result.
hcaCardinalities<-function(h) {
    # m = # of clusters
    m<-length(h$height)
    cards<-rep(0,m)
    for (i in 1:m) {
        c1<-ifelse(h$merge[i,1]<0,1,cards[h$merge[i,1]])
        c2<-ifelse(h$merge[i,2]<0,1,cards[h$merge[i,2]])
        cards[i]<-c1+c2
    }
    return(cards)
}

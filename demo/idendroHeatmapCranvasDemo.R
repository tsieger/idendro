## Demo showing `idendro' bidirectional communication between idendro
## and cranvas qscatter plot.
##

require(cranvas)

n<-10
x<-cbind(x1=1:10,x2=c(1,2,3,4,5,9,9,9,9,9))
rownames(x)<-1:n
cat('computing dist\n')
dx<-dist(x)
cat('computing HCA\n')
hx<-hclust(dx)

qx<-qdata(x)
qx$.size<-jitter(qx$.size,.1)
print(qscatter(x1,x2,data=qx,xlim=c(0,11),ylim=c(0,10)))

cat('plotting HCA\n')
idendro(hx,qx)

## Demo showing `idendro' using spectroscopy data, with cranvas scatter
## plot and parallel coordinate plots integrated.
##

require (hyperSpec) # chondro data
data(chondro)
# extract data matrix
x <- chondro [[]]
# prepend 'x' in front of numeric dimnames
colnames(x)<-paste('x',colnames(x),sep='')
# HCA
dx<-dist(x)
hx<-hclust(dx)
# mutable data frame construction
qx<-qdata(x)
idendro(hx,qx,maxClusterCount=6)
# scatter plot
print(qscatter(x602,x606,data=qx,unibrushcolor=FALSE))
# parallel coordinate plot
print(qparallel(~.,data=qx))

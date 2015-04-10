## Visualization of spectroscopy data in dendrogram and heat map with
## cranvas scatter plot and parallel coordinate plot integrated.
##

library(hyperSpec) # chondro data

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

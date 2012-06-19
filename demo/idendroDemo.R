n<-10
x<-cbind(rnorm(n),rnorm(n))
cat('computing dist\n')
dx<-dist(x)
cat('computing HCA\n')
hx<-hclust(dx)
cat('plotting HCA\n')
idendro(hx,x)
cat('done\n')

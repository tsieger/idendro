## Test different types of the data matrix `x' argument to idendro.
##

test_that('different types of x argument work',{
    require(idendro)

    x<-1:10
    idendro(hclust(dist(x)),x)

    x<-as.character(1:10)
    idendro(hclust(dist(x)),x)

    x<-matrix(1:10,10)
    idendro(hclust(dist(x)),x)

    x<-data.frame(1:10)
    idendro(hclust(dist(x)),x)
})

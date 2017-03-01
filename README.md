# idendro

'idendro' is an interactive dendrogram that enables the user to select 
and color clusters, to zoom and pan the dendrogram, and to visualize 
the clustered data not only in a built-in heat map, but also in any
interactive plot implemented in the 
[cranvas](https://github.com/ggobi/cranvas) package.

Note: there is an alternative lightweight version of 'idendro' called
[idendr0](https://github.com/tsieger/idendr0). It is implemented in
terms of base R graphics and platform-independent Tcl/Tk GUI, and
integrated with the [GGobi](http://ggobi.org/) dynamic interactive graphics.
[idendr0](https://github.com/tsieger/idendr0) is suitable for users who 
have no [cranvas](https://github.com/ggobi/cranvas) package installed.

#### Documentation

[A paper](http://dx.doi.org/10.18637/jss.v076.i10) in the 
[Journal of Statistical Software](http://jstatsoft.org/)
and [idendro vignette](/inst/doc/idendro.pdf).

## To install:

* the latest development version: 
  `devtools::install_github("tsieger/idendro", args="--no-multiarch")`

The installation of prerequisities is described in the full installation
instructions available at https://github.com/tsieger/idendro/wiki.

#### A simple example:

    library(idendro)
    hc <- hclust(dist(iris[, 1:4]))
    idendro(hc, iris)

![Example](/man/figures/idendro1.png?raw=true "Simple example.")

#### Bidirectional integration with interactive cranvas plots:

    library(idendro)
    library(cranvas)
    hc <- hclust(dist(iris[, 1:4]))
    qx <- qdata(iris)
    idendro(hx, qx)
    print(qscatter(Sepal.Length, Sepal.Width, data = qx))
    print(qparallel(~., data = qx))

![Example](/man/figures/idendro2.png?raw=true "Integration with cranvas.")

For demos, please run `demo(package="idendro")`.

Find out more at https://github.com/tsieger/idendro.

# idendro

'idendro' is an interactive dendrogram that enables the user to select 
and color clusters, to zoom and pan the dendrogram, and to visualize 
the clustered data not only in a built-in heat map, but also in any
interactive plot implemented in the 'cranvas' package.

Note: there is an alternative lightweight 'idendro' version called
'idendr0' (https://github.com/tsieger/idendr0).  It is implemented in
terms of base R graphics and platform-independent Tcl/Tk GUI, and
integrated with the GGobi dynamic interactive graphics. 'idendr0' is
suitable for users who have no 'cranvas' package installed.

To install:

* the latest development version: 
  `install_github("tsieger/idendro", args="--no-multiarch")`

The installation of prerequisities is described in the full installation
instructions available at https://github.com/tsieger/idendro/wiki.

A simple example:

    hc <- hclust(dist(iris[, 1:4]))
    idendro(hc, iris)

For demos, please run `demo(package="idendro")'.

Find out more at https://github.com/tsieger/idendro
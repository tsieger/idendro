\name{hca5000}
\alias{hca5000}
\docType{data}
\title{
idendro demo data
}
\description{
A list of 'x' and 'hx' elements.
'x' is a matrix of 5000 rows (observations) by 3 columns (dimensions).
'hx' is an object of class 'hclust' containing the result of
hierarchical cluster analysis performed on 'x'.
}
\usage{data(hca5000)}
\examples{
data(hca5000)
idendro(hca5000$hx,hca5000$x,observationAnnotationEnabled=FALSE)
}
\keyword{datasets}

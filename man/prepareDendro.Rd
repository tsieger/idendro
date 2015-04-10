\name{prepareDendro}
\alias{prepareDendro}
\title{prepareDendro}
\description{Perform all computations needed to display a dendrogram and
initialize a shared data frame.

This is an internal function not to be called by the user.}
\usage{prepareDendro(h, x = NULL, xOrig = NULL, doFlipG = TRUE, dbg = 0)}
\arguments{
  \item{h}{an object of class 'stats::hclust' describing a clustering}
  \item{x}{a data frame holding observations tha were clustered
giving rise to 'h', converted to numeric, and scaled}
  \item{xOrig}{a data frame holding observations tha were clustered
giving rise to 'h'}
  \item{doFlipG}{should branches' heights ("grow") be flipped
such that the heights of elementary observations correspond not
to 0, but to the height of the dendrogram?}
  \item{dbg}{debug verbosity level}
}

\value{shared data frame}

\author{Tomas Sieger}






\keyword{internal}

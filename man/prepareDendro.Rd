\name{prepareDendro}
\alias{prepareDendro}
\title{prepareDendro}
\description{Perform all computations needed to display a dendrogram and
initialize a shared data frame.

This is an internal function not to be called by the user.}
\usage{prepareDendro(h, x = NULL, dbg = 0)}
\arguments{
  \item{h}{an object of class 'stats::hclust' describing a clustering}
  \item{x}{a data frame holding observations tha were clustered
giving rise to 'h'}
  \item{dbg}{debug control}
}

\value{shared data frame}

\author{Tomas Sieger}





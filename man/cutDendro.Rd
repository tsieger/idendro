\name{cutDendro}
\alias{cutDendro}
\title{cutDendro}
\description{Select clusters by cutting the current zoom of the dendrogram at a
specified height.}
\usage{cutDendro(df, cutG, dendroZoom, dbg = FALSE)}
\arguments{
  \item{df}{shared data frame}
  \item{cutG}{cutting grow height}
  \item{dendroZoom}{current dendro zoom region}
  \item{dbg}{debug level}
}

\value{A list of shared data frame 'df' with new cluster selection,
and the number of clusters selected.}

\author{Tomas Sieger}






\keyword{internal}

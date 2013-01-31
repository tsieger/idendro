\name{smoothHeatmap}
\alias{smoothHeatmap}
\title{smoothHeatmap}
\description{Smooth heatmap by averaging data over observations in clusters.}
\usage{smoothHeatmap(x, ch, dbg.heatmap = 0)}
\arguments{
  \item{x}{a data frame holding observations tha were clustered}
  \item{ch}{number of clusters specifying the amount of smoothing:
observations in clusters will get smoothed together, the value
of 'n' specifies no smoothing while the value of 1 would lead
to maximal smoothing}
  \item{dbg.heatmap}{debug verbosity level}
}

\value{a data frame holding smoothed observations}

\author{Tomas Sieger}






\keyword{internal}

\name{unselectAllClusters}
\alias{unselectAllClusters}
\title{unselectAllClusters}
\description{Unselect all clusters.}
\usage{unselectAllClusters(df, dbg)}
\arguments{
  \item{df}{shared data frame}
  \item{dbg}{debug flag/level}
}

\value{a list of a shared data frame 'df' and a boolean flag
'selectionChanged' determing if cluster selection has changed
(so the caller can learn whether to redraw clusters).}

\author{Tomas Sieger}






\keyword{internal}

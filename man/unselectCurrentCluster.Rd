\name{unselectCurrentCluster}
\alias{unselectCurrentCluster}
\title{unselectCurrentCluster}
\description{Unselect the current cluster.}
\usage{unselectCurrentCluster(df, dbg = FALSE)}
\arguments{
  \item{df}{shared data frame}
  \item{dbg}{debug flag/level}
}

\value{a list of shared data frame 'df' and a boolean flag
'selectionChanged' determing if clsuter selection has changed
(so the caller can learn whether to redraw clusters).}

\author{Tomas Sieger}






\keyword{internal}

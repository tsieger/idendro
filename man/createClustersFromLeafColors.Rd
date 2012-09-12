\name{createClustersFromLeafColors}
\alias{createClustersFromLeafColors}
\title{createClustersFromLeafColors}
\description{Initialize clusters (i.e. cluster selection) from leaf colors (i.e.
assignment of some observations to some clusters)}
\usage{createClustersFromLeafColors(df, leafColors, maxClusterCount, 
    dbg = 0)}
\arguments{
  \item{df}{shared data frame, see 'prepareDendro'}
  \item{leafColors}{leaf color indices, i.e. a nonnegative number
assigned to each observation. 0 means given observation is not
member of any cluster, a value i>0 means given observation is a
member of cluster 'i'.}
  \item{maxClusterCount}{max cluster count}
  \item{dbg}{debug verbosity level}
}

\value{shared data frame 'df' with cluster selection based on 'leafColors'}

\author{Tomas Sieger}






\keyword{internal}

\name{mySeq}
\alias{mySeq}
\title{mySeq}
\description{Sequence generation resembling matlab ':' operator.
The difference from R `seq' is the behaviour in case when
from=2, to=1 and b=1 - in that case R seq raises an error, while
matlab returns an empty sequence.
This function returns an empty sequence as well in this case.}
\usage{mySeq(from, to, by = 1)}
\arguments{
  \item{from}{starting value}
  \item{to}{ending value}
  \item{by}{step (defaults to 1)}
}



\author{Tomas Sieger}






\keyword{internal}

\name{groupsum}
\alias{groupsum}
\title{Extension of rowsum...}
\usage{groupsum(x, group, dim=1, reorder=TRUE, na.rm=FALSE, ..., drop=!is.array(x))
}
\description{Extension of \code{rowsum}}
\details{\code{groupsum}: \code{groupsum} extends \code{\link{rowsum}}: it allows \code{group} to be an array of the same shape
as \code{x}.

}
\value{\code{groupsum}: like \code{\link[base]{rowsum}}, but further dimensions of the array are preserved.

}
\author{Claudia Beleites}
\seealso{\code{\link[base]{rowsum}} \code{\link{rowsum}}}
\keyword{array}
\keyword{algebra}
\keyword{arith}
\arguments{\item{x}{array to be \code{rowsum}med}
\item{group}{grouping variable (integer or factor) indicating groups of samples. \code{}}
\item{dim}{along which dimension should the group sums be taken? (default: rows)}
\item{reorder}{should the groups be ordered? see \code{\link[base]{rowsum}}}
\item{na.rm}{shoud \code{NA}s be removed?}
\item{...}{ignored}
\item{drop}{should 1d arrays drop to vectors?}
}
\alias{groupsum-methods}
\alias{groupsum}


\name{rowsum,array-method}
\alias{rowsum,array-method}
\alias{rowsum}
\title{rowsum for arrays...}
\usage{\S4method{rowsum}{array}(x, group, reorder=TRUE, na.rm = FALSE, ...)}
\description{rowsum for arrays}
\details{This function extends the base function \code{\link[base]{rowsum}}.}
\value{like \code{\link[base]{rowsum}}, but further dimensions of the array are preserved.}
\author{Claudia Beleites}
\seealso{\code{\link[base]{rowsum}}}
\keyword{array}
\keyword{algebra}
\keyword{arith}
\docType{methods}
\arguments{\item{x}{array to be \code{rowsum}med}
\item{group}{grouping variable (integer or factor) indicating groups of samples. \code{}}
\item{reorder}{should the groups be ordered? see \code{\link[base]{rowsum}}}
\item{na.rm}{shoud \code{NA}s be removed?}
\item{...}{ignored}
}


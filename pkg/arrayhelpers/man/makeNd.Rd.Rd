\name{makeNd.Rd}
\alias{restoredim}
\title{restoredim restores the shape the array had before makeNd was called.}
\usage{restoredim(a, old=attr(a, "old"), n=1)
}
\description{\code{restoredim} restores the shape the array had before \code{makeNd} was called.}
\details{Attributes \code{old.dim} and \code{old.dimnames} are used by default. \code{restoredim} is the
inverse of \code{makeNd}.

Note that missing attributes as well as \code{old.dim = NULL} produce a (dimensionless)
vector. This is also the case if \code{a} lost the \code{old.*} attributes during 
computations like \code{as.numeric}, \code{c}, etc..}
\value{an array}
\author{Claudia}
\arguments{\item{a}{an array}
\item{old}{list containing a list with (possibly) elements \code{dim}, \code{dimnames}, and
\code{names}. The last element of this list is used.}
\item{n}{how many makeNdim steps to go back?}
}
\examples{a <- array (1 : 24, 4 : 3)
a
restoredim (makeNd (a, 0))

x <- makeNd (a, 0)
attr (x, "old")}


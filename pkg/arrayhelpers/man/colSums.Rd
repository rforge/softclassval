\name{colSums}
\alias{colSums,matrix-method}
\alias{colSums}
\title{Row and column sums and means for numeric arrays.}
\description{Row and column sums and means for numeric arrays.}
\details{\code{colSums,matrix-method}: These functions extend the respective base functions by (optionally) preserving the shape of the
array (i.e. the summed dimensions have length 1).

}
\value{\code{colSums,matrix-method}: like \code{\link[base]{colSums}} if \code{drop = TRUE}, otherwise an array where the
summed dimensions have length 1.

}
\author{Claudia Beleites}
\seealso{\code{\link[base]{colSums}}}
\keyword{array}
\keyword{algebra}
\keyword{arith}
\docType{methods}
\arguments{\item{x}{an array of two or more dimensions, containing numeric, complex, integer or logical
values, or a numeric data frame.}
\item{na.rm}{logical indicating treatment of missing values}
\item{dims}{integer: Which dimensions are regarded as \sQuote{rows} or \sQuote{columns} to sum
over.  For \code{row*}, the sum or mean is  over dimensions \code{dims + 1, \dots}; for \code{col*}
it is over  dimensions \code{1 : dims}.}
\item{...}{the \code{signature = "AsIs"} methods hand on all parameters}
\item{drop}{If \code{FALSE}, the number of dimensions is retained: the length of the dimensions
that are summed or averaged is set to  1. \code{TRUE} yield the same behaviour as
\code{\link[base]{colSums}}}
}
\examples{a <- array (1 : 24, 4 : 2)
a

rowSums (a)
rowSums (a, drop = FALSE)

colSums (a)
colSums (a, drop = FALSE)

colSums (a, dim = 2)
colSums (a, dim = 2, drop = FALSE)}
\alias{colSums.AsIs}
\alias{colSums,array-method}
\alias{colSums}
\alias{colMeans,matrix-method}
\alias{colMeans}
\alias{colMeans.AsIs}
\alias{colMeans,array-method}
\alias{colMeans}
\alias{rowSums,matrix-method}
\alias{rowSums}
\alias{rowSums.AsIs}
\alias{rowSums,array-method}
\alias{rowSums}
\alias{rowMeans,matrix-method}
\alias{rowMeans}
\alias{rowMeans.AsIs}
\alias{rowMeans,array-method}
\alias{rowMeans}


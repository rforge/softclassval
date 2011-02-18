\name{makeNd}
\alias{makeNd}
\title{Ensure/collapse an array into n dimensions and restore the old dimensions...}
\usage{makeNd(a, N)
}
\description{Ensure/collapse an array into \code{n} dimensions and restore the old dimensions}
\details{\code{nameNd} ensures a given number of dimensions:
If \code{a} has less than \code{N} dimensions, new dimensions of length 1 are appended.
If \code{a} has more than \code{N} dimensions, the supernumerary dimensions are collapsed onto
the last dimension.}
\value{N-dimensional array}
\author{Claudia Beleites}
\arguments{\item{a}{an array (matrix, vector)}
\item{N}{the desired number of dimensions, 0 to remove the \code{dim} and \code{dimnames}
attributes (i.e. to create a vector).}
}
\examples{v <- arrayhelpers:::v
v
makeNd (v, 1L)
dim (makeNd (v, 1L))
dim (makeNd (v, 3L))

m <- arrayhelpers:::m
m
makeNd (m, 1L)
dim (makeNd (m, 1L))
makeNd (m, 0L) 
dim (makeNd (m, 0L))
makeNd (m, 3L)

a <- arrayhelpers:::a
a
dim (makeNd (a, 1L))
dim (makeNd (a, 0L))
makeNd (a,  2L)          
makeNd (a, -2L)
makeNd (a, -4L)
makeNd (a, 3L)}


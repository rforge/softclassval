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
makeNd (v, 1)
dim (makeNd (v, 1))
dim (makeNd (v, 3))

m <- arrayhelpers:::m
m
makeNd (m, 1)
dim (makeNd (m, 1))
makeNd (m, 0) 
dim (makeNd (m, 0))
makeNd (m, 3)

a <- arrayhelpers:::a
a
dim (makeNd (a, 1))
dim (makeNd (a, 0))
makeNd (a,  2)          
makeNd (a, -2)
makeNd (a, -4)
makeNd (a, 3)}


\name{ta}
\alias{ta}
\title{Transpose arrays...}
\usage{ta(x)
}
\description{Transpose arrays}
\details{This function provides transposing of arrays or vectors as swapping their first two dimensions.
\code{t (array)} can be enabled via \code{\link[methods]{setMethod}}, see the example.}
\value{the array with the first two dimensions swapped.}
\author{Claudia Beleites}
\seealso{\code{\link[base]{t}}}
\arguments{\item{x}{an array}
}
\examples{a <- array (1 : 24, 4:2)
a
ta (a)

setMethod ("t", "array", ta)
t (a)
removeMethod ("t", "array")}


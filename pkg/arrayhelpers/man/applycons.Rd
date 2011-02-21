\name{applycons}
\alias{applycons}
\title{Conservative version of apply: tries to preserve the dimensions of the original array...}
\usage{applycons(X, MARGIN, FUN, ...)
}
\description{Conservative version of apply: tries to preserve the dimensions of the original array}
\details{\code{\link[base]{apply}} returns its results as an array of shape (not-margin dimensions *
length of FUN's result) x MARGIN dimensions.

\code{applycons} rearranges these results, so that

1. the result dimensions are appended rather than prepended, and
2. tries to preserve the shape of the}
\value{array}
\author{Claudia Beleites}
\seealso{\code{\link[base]{apply}}}
\arguments{\item{X}{an array or matrix.}
\item{MARGIN}{a vector giving the subscripts which the function will be applied over. E.g., for a
matrix \code{1} indicates rows, \code{2} indicates columns, \code{c(1, 2)} indicates rows and
columns. Where \code{X} has named dimnames, it can be a character vector selecting dimension
names.}
\item{FUN}{the function to be applied: see \sQuote{Details}. In the case of functions like
\code{+}, \code{\%*\%}, etc., the  function name must be backquoted or quoted.}
\item{...}{optional arguments to \code{FUN}.}
}


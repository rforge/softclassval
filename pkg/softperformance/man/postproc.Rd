\name{postproc}
\alias{postproc}
\title{Attach postprocessing function to operator...}
\usage{postproc(op)
}
\description{Attach postprocessing function to operator}
\details{The postprocessing function is applied during performance calculation after averaging but before
\code{\link{dev}} is applied. This is the place where the root is taken of root mean squared errors.

\code{postproc (op)} retrieves the postprocessing function (or \code{NULL} if none is attached)}
\value{logical indicating the type of operator. \code{NA} if the attribute is missing.}
\author{Claudia Beleites}
\references{\Sexpr{cite ("softperformance")}}
\seealso{\code{\link{sens}} \code{\link{post}}}
\arguments{\item{op}{the operator (function)}
}
\examples{postproc (wRMSE)
myop <- function (r, p) p * (r == 1)
postproc (myop) <- `sqrt`}


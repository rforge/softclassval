\name{postproc}
\alias{postproc}
\title{Attach postprocessing function to operator...}
\usage{postproc(op)

postproc (op) <- value
}
\description{Attach postprocessing function to operator}
\details{\code{postproc}: The postprocessing function is applied during performance calculation after averaging but before
\code{\link{dev}} is applied. This is the place where the root is taken of root mean squared errors.

\code{postproc (op)} retrieves the postprocessing function (or \code{NULL} if none is attached)

}
\value{\code{postproc}: logical indicating the type of operator. \code{NA} if the attribute is missing.

}
\author{Claudia Beleites}
\seealso{\code{\link{sens}} \code{\link{post}}}
\arguments{\item{op}{the operator (function)}
\item{value}{function (or its name or symbol) to do the post-processing. \code{NULL} deletes the
postprocessing function.}
}
\examples{
postproc (wRMSE)
myop <- function (r, p) p * (r == 1)
postproc (myop) <- `sqrt`
}
\alias{postproc<-}


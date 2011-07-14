\name{dev}
\alias{dev}
\title{Mark operator as deviation measure...}
\usage{dev(op)
dev (op) <- value}
\description{Mark operator as deviation measure}
\details{\code{dev}: The operators measure either a performance (i.e. accordance between reference and prediction) or
a deviation. \code{dev (op) == TRUE} marks operators measuring deviation.

}
\value{\code{dev}: logical indicating the type of operator. \code{NULL} if the attribute is missing.

}
\author{Claudia Beleites}
\seealso{\code{\link{sens}} \code{\link{post}}}
\arguments{\item{op}{the operator (function)}
\item{value}{logical indicating the operator type}
}
\examples{dev (wRMSE)
myop <- function (r, p) p * (r == 1)
dev (myop) <- TRUE}
\alias{dev<-}


\name{hard}
\alias{hard}
\title{Mark operator as hard measure...}
\usage{hard(op)
hard (op) <- value}
\description{Mark operator as hard measure}
\details{\code{hard}: The operators may work only for hard classes (see \code{\link[softclassval:operators]{and}}). \code{hard (op)
== TRUE} marks hard operators.

}
\value{\code{hard}: logical indicating the type of operator. \code{NULL} if the attribute is missing.

}
\author{Claudia Beleites}
\seealso{\code{\link{sens}} \code{\link[softclassval:operators]{and}}}
\arguments{\item{op}{the operator (function)}
\item{value}{logical indicating the operator type}
}
\examples{hard (and)
myop <- function (r, p) p * (r == 1)
hard (myop) <- TRUE}
\alias{hard<-}


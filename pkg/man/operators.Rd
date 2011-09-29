\name{operators}
\alias{strong}
\title{And (conjunction) operators...}
\usage{strong(r, p)

weak(r, p)


prd(r, p)


and(r, p)


wMAE(r, p)


wMSE(r, p)

}
\encoding{UTF8}
\description{And (conjunction) operators}
\details{\code{strong}: And operators for the soft performance calculation.

The predefined operators are:
\tabular{llllll}{
Name         \tab Definition                 \tab \code{\link{dev}}? \tab \code{\link{postproc}}?  \tab \code{\link{hard}}? \tab Explanation                                                           \cr
\code{weak} == \code{gdl}   \tab \code{pmin (r, p)}         \tab FALSE              \tab FALSE                    \tab FALSE               \tab the \enc{Gödel}{Goedel}-operator (weak conjunction)                   \cr
\code{strong} == \code{luk}   \tab \code{pmax (r + p - 1, 0)} \tab FALSE              \tab FALSE                    \tab FALSE               \tab \enc{Łukasiewicz}{Lukasiewicz}-operator (strong conjunction)          \cr
\code{prd}   \tab \code{r * p}               \tab FALSE              \tab FALSE                    \tab FALSE               \tab product operator                                                      \cr
\code{and}   \tab \code{r * p}               \tab FALSE              \tab FALSE                    \tab TRUE                \tab Boolean conjunction: accepts only 0 or 1, otherwise yields \code{NA}  \cr
\code{wMAE}  \tab \code{r * abs (r - p)}     \tab TRUE               \tab FALSE                    \tab FALSE               \tab for weighted mean absolute error                                      \cr
\code{wMSE}  \tab \code{r * (r - p)^2}       \tab TRUE               \tab FALSE                    \tab FALSE               \tab for weighted mean squared error                                       \cr
\code{wRMSE} \tab \code{r * (r - p)^2}       \tab TRUE               \tab TRUE                     \tab FALSE               \tab for root weighted mean squared error                                  \cr
}

}
\value{\code{strong}: numeric of the same size as p

}
\author{Claudia Beleites}
\seealso{Performance measures: \code{\link{sens}}}
\references{see the literature in \code{citation ("softclassval")}}
\arguments{\item{p}{prediction vector, matrix, or array with numeric values in [0, 1], for \code{and} in \{0, 1\}}
\item{r}{reference vector, matrix, or array with numeric values in [0, 1], for \code{and} in \{0, 1\}}
}
\examples{
ops <- c ("strong", "weak", "prd", "and", "wMAE", "wMSE", "wRMSE")

## make a nice table


lastline <- function (f){
body <- deparse (body (get (f))) ## function body
body <- body [!grepl ("^[ \t]*[}][ \t]*$", body)]
gsub ("^[ \t]+([^ \t].*[^ \t])[ \t]*$", "\\1", tail (body, 1))
}

data.frame (source = sapply (ops, lastline),
dev = sapply (ops, function (f) dev (get (f))),
hard = sapply (ops, function (f) hard (get (f))),
postproc = I (lapply (ops, function (f) postproc (get (f))))
)

x <- softclassval:::v
x

luk (0.7, 0.8)

## The behaviour of the operators
## op (x, 1)
cbind (x, sapply (c ("strong", "weak", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (x, 1), x))

## op (x, 0)
cbind (x, sapply (c ("strong", "weak", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (x, 0), x))

## op (x, x)
cbind (x, sapply (c ("strong", "weak", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (x, x), x))


## Note that the deviation operators are not commutative
## (due to the weighting by reference)
zapsmall (
cbind (sapply (c ("strong", "weak", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (1, x), x)) -
cbind (sapply (c ("strong", "weak", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (x, 1), x)) 
)
}
\alias{luk}
\alias{weak}
\alias{gdl}
\alias{prd}
\alias{and}
\alias{wMAE}
\alias{wMSE}
\alias{wRMSE}


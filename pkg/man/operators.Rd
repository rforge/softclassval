\name{operators}
\alias{luk}
\title{And (conjunction) operators...}
\usage{luk(r, p)
gdl(r, p)
prd(r, p)
and(r, p)
wMAE(r, p)
wMSE(r, p)
}
\encoding{UTF-8}
\description{And (conjunction) operators}
\details{\code{luk}: And operators for the soft performance calculation.

The predefined operators are:
\tabular{lllll}{
Name         \tab Definition                 \tab \code{\link{dev}}? \tab \code{\link{postproc}}? \tab Explanation                                                  \cr
\code{gdl}   \tab \code{pmin (r, p)}         \tab FALSE              \tab FALSE                   \tab the \enc{Gödel}{Goedel}-operator (weak conjunction)          \cr
\code{luk}   \tab \code{pmax (r + p - 1, 0)} \tab FALSE              \tab FALSE                   \tab \enc{Łukasiewicz}{Lukasiewicz}-operator (strong conjunction) \cr
\code{prd}   \tab \code{r * p}               \tab FALSE              \tab FALSE                   \tab product operator                                             \cr
\code{and}   \tab \code{r * p}               \tab FALSE              \tab FALSE                   \tab Boolean conjunction: accepts only 0 or 1, otherwise yields \code{NA}  \cr
\code{wMAE}  \tab \code{r * abs (r - p)}     \tab TRUE               \tab FALSE                   \tab for weighted mean absolute error                                 \cr
\code{wMSE}  \tab \code{r * (r - p)^2}       \tab TRUE               \tab FALSE                   \tab for weighted mean squared error                                  \cr
\code{wRMSE} \tab \code{r * (r - p)^2}       \tab TRUE               \tab TRUE                    \tab for root weighted mean squared error                                  \cr
}

}
\alias{luk}
\alias{gdl}
\alias{prd}
\alias{and}
\alias{wMAE}
\alias{wMSE}
\alias{wRMSE}
\alias{operators}
\value{\code{luk}: numeric of the same size as p

}
\author{Claudia Beleites}
\seealso{Performance measures: \code{\link{sens}}}
\references{see the literature in \code{citation ("softclassval")}}
\arguments{\item{p}{prediction vector, matrix, or array with numeric values in [0, 1]}
\item{r}{reference vector, matrix, or array with numeric values in [0, 1]}
}
\examples{ops <- c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE")

## make a nice table
t (sapply (ops, function (x){
x <- c (attributes (get (x)) [c ("source", "dev", "postproc")])
names (x) <- c ("source", "dev", "postproc")
x
}))


x <- softclassval:::v
x

luk (0.7, 0.8)

## The behaviour of the operators
## op (x, 1)
cbind (x, sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (x, 1), x))

## op (x, 0)
cbind (x, sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (x, 0), x))

## op (x, x)
cbind (x, sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (x, x), x))


## Note that the deviation operators are not commutative
## (due to the weighting by reference)
zapsmall (
cbind (sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (1, x), x)) -
cbind (sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
function (op, x) get (op) (x, 1), x)) 
)}
\alias{gdl}
\alias{prd}
\alias{and}
\alias{wMAE}
\alias{wMSE}
\alias{wRMSE}


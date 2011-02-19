\name{performance}
\alias{sens}
\title{Performance calculation for soft classification...}
\usage{sens(..., r, p, group, operator=prd, dev=dev(operator),
    postproc=postproc(operator), eps=1e-08)
}
\description{Performance calculation for soft classification}
\details{These performance measures can be used with prediction and reference being continuous class
memberships in [0, 1].

The rows of \code{r} and \code{p} are considered the samples, columns will usually hold the
classes, and further dimensions are preserved but ignored.

\code{r} must have the same number of rows as \code{p}, all other dimensions may be filled by recycling. If the c}
\value{numeric of size (ngroups x \code{dim (p) [-1L]})}
\author{Claudia Beleites}
\seealso{Performance measures: \code{\link{sens}}}
\references{see the literature in \code{citation ("softperformance")}}
\arguments{\item{...}{ensures that arguments are named}
\item{r}{vector, matrix, or array with reference.}
\item{p}{vector, matrix, or array with predictions}
\item{group}{grouping variable for the averaging by \code{\link[base]{rowsum}}. If \code{NULL},
all samples (rows) are averaged.}
\item{operator}{the \code{\link[softperformance]{operators}} to be used}
\item{dev}{does the operator measure deviation?}
\item{postproc}{if a post-processing function is needed after averaging, it can be given here. See the example.}
\item{eps}{limit below which denominator is considered 0}
}


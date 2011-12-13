\name{checkrp}
\alias{checkrp}
\title{Input checks for performance calculation}
\usage{
  checkrp(r, p)
}
\arguments{
  \item{r}{reference}

  \item{p}{prediction}
}
\value{
  \code{r}, possibly recycled to length of \code{p} or with
  dimensions shortened to \code{p}.
}
\description{
  Checks whether \code{r} and \code{p} are valid reference
  and predictions. If \code{p} is a multiple of \code{r},
  recycles \code{r} to the size and shape of \code{p}. If
  \code{r} has additional length 1 dimensions (usually
  because dimensions were dropped from \code{p}), it is
  shortend to the shape of \code{p}.
}
\details{
  In addition, any \code{NA}s in \code{p} are transferred
  to \code{r} so that these samples are excluded from
  counting in \code{\link{nsamples}}.
}
\author{
  Claudia Beleites
}


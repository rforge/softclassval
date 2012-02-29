\name{hardclasses}
\alias{hardclasses}
\title{Convert to hard class labels}
\usage{
  hardclasses(x, classdim = 2L, soft.name = NA,
    tol = 1e-05, drop = TRUE)
}
\arguments{
  \item{x}{matrix or array holding the class memberships}

  \item{classdim}{dimension that holds the classes, default
  columns}

  \item{soft.name}{level for soft samples}

  \item{tol}{tolerance: samples with membership >= 1 - tol
  are considered to be hard samples of the respective
  class.}

  \item{drop}{see \code{\link[arrayhelpers]{drop1d}}}
}
\value{
  factor array of shape \code{dim (x) [-classdim]}
}
\description{
  Converts the soft class labels in \code{x} into a factor
  with hard class memberships and \code{NA} for soft
  samples.
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[softclassval]{factor2matrix}} for the inverse
}


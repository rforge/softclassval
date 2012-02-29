\name{factor2matrix}
\alias{factor2matrix}
\title{Convert hard class labels to membership matrix}
\usage{
  factor2matrix(f)
}
\arguments{
  \item{f}{factor with class labels}
}
\value{
  matrix of size \code{length (f)} x \code{nlevels (f)}
}
\description{
  Converts a factor with hard class memberships into a
  membership matrix
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[softclassval]{hardclasses}} for the inverse
}


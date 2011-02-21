\name{delold}
\alias{delold}
\title{Strip the attributes keeping track of the former shape...}
\usage{delold(a)
}
\description{Strip the attributes keeping track of the former shape}
\details{Convenient for printing}
\value{\code{a} stripped of the \code{old.*} attributes.}
\author{Claudia Beleites}
\arguments{\item{a}{the array}
}
\examples{a <- arrayhelpers:::a
makeNd (a, 2)
delold (makeNd (a, 2))}


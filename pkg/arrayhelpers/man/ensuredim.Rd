\name{ensuredim}
\alias{ensuredim}
\title{Enforce array and convert to vector if appropriate...}
\usage{ensuredim(x)
drop1d(x, drop=TRUE)
}
\description{Enforce array and convert to vector if appropriate}
\details{\code{ensuredim}: \code{ensuredim} turns vectors into 1d-arrays, and leaves arrays unchanged. \code{drop1d} is the
inverse: it converts 1d arrays into vectors.

}
\value{\code{ensuredim}: \code{esuredim} array of at least one dimension

\code{drop1d}: \code{drop1d} vector, if \code{x} had only 1 dimension


}
\author{Claudia Beleites}
\arguments{\item{x}{vector (or array)}
\item{drop}{if \code{TRUE}, 1d arrays are converted into vectors}
}
\alias{drop1d}


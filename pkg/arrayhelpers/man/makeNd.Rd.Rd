\name{makeNd.Rd}
\alias{restoredim}
\title{restoredim restores the shape the array had before makeNd was called.}
\usage{restoredim(a, old, n=1, ..., usedim=TRUE, fromend=FALSE, drop=FALSE)
}
\description{\code{restoredim} restores the shape the array had before \code{makeNd} was called.}
\details{Attributes \code{old.dim} and \code{old.dimnames} are used by default. \code{restoredim} is the
inverse of \code{makeNd}.

Note that missing attributes as well as \code{old.dim = NULL} produce a (dimensionless)
vector. This is also the case if \code{a} lost the \code{old.*} attributes during 
computations like \code{as.numeric}, \code{c}, etc..

\code{fromend} together with numeric \code{usedim} specifies dimensions counting from the
end. E.g. \code{fromend = TRUE} and \code{usedim = 1 : 3} for an array to be restored to 10d
means restoring dimensions 8 : 10. \code{fromend = TRUE} and \code{usedim = -(1 : 3)} restores
dimensions 1 to 7.}
\value{an array}
\author{Claudia}
\arguments{\item{a}{an array}
\item{old}{list containing a list with (possibly) elements \code{dim}, \code{dimnames}, and
\code{names}. The nth last element of this list is used.}
\item{n}{how many makeNdim steps to go back?}
\item{...}{ignored}
\item{usedim}{use only the specified dimensions}
\item{fromend}{if \code{TRUE}, numeric \code{usedim} are counted from the end, see details.}
\item{drop}{should 1d arrays drop to vectors?}
}
\examples{a <- array (1 : 24, 4 : 3)
a
restoredim (makeNd (a, 0))

x <- makeNd (a, 0)
attr (x, "old")}


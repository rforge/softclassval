##' \code{restoredim} restores the shape the array had before \code{makeNd} was called.
##'
##' Attributes \code{old.dim} and \code{old.dimnames} are used by default. \code{restoredim} is the
##' inverse of \code{makeNd}.
##'
##' Note that missing attributes as well as \code{old.dim = NULL} produce a (dimensionless)
##' vector. This is also the case if \code{a} lost the \code{old.*} attributes during 
##' computations like \code{as.numeric}, \code{c}, etc..
##' @param a an array
##' @param old list containing a list with (possibly) elements \code{dim}, \code{dimnames}, and
##' \code{names}. The last element of this list is used.
##' @param n how many makeNdim steps to go back?
##' @return an array
##' @author Claudia
##' @rdname makeNd.Rd
##' @export
##' @examples
##'
##' a <- array (1 : 24, 4 : 3)
##' a
##' restoredim (makeNd (a, 0))
##'
##' x <- makeNd (a, 0)
##' attr (x, "old")
##' 
restoredim <- function (a, old = attr (a, "old"), n = 1L, usedim = TRUE, drop = FALSE){
  old <- peek (a, "old", n = n)
  a <- pop (a, "old", n = n)

  dim <-  old$dim [usedim]
  dimnames <-  old$dimnames [usedim]
  names <-  old$names [usedim]

  if (length (dim) == 1L && drop){
    dim (a) <- NULL
    dimnames (a) <- NULL
    names (a) <- dimnames
  } else {
    dim (a) <- dim
    dimnames (a) <- dimnames
    names (a) <- NULL
  }
  a
}

##' @nord
.test (restoredim) <- function (){
  checkIdentical (a, restoredim (makeNd (a,  5)))
  checkIdentical (a, restoredim (makeNd (a,  0)))
  
  checkIdentical (a, restoredim (makeNd (a, -5)))
  checkIdentical (a, restoredim (makeNd (a, -2)))

  checkIdentical (v, restoredim (makeNd (v,  0)))
  checkIdentical (v, restoredim (makeNd (v,  1)))
  checkIdentical (v, restoredim (makeNd (v, -1)))
  
  checkIdentical (dim (restoredim (as.numeric (makeNd (a, 0L)))), NULL)

  checkIdentical (a, restoredim (restoredim (makeNd (makeNd (a,  5), 0))))
  checkIdentical (a, restoredim (makeNd (makeNd (a,  5), 0), n = 2L))

  checkIdentical (a, restoredim (makeNd (makeNd (a,  5), 0), n = 3L)) # OK

  warn <- options(warn = 2)$warn
  on.exit (options (warn = warn))
  checkException (restoredim (makeNd (makeNd (a,  5), 0), n = 3L))
}


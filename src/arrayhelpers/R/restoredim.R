##' \code{restoredim} restores the shape the array had before \code{makeNd} was called.
##'
##' Attributes \code{old.dim} and \code{old.dimnames} are used by default. \code{restoredim} is the
##' inverse of \code{makeNd}.
##'
##' Note that missing attributes as well as \code{old.dim = NULL} produce a (dimensionless)
##' vector. This is also the case if \code{a} lost the \code{old.*} attributes during 
##' computations like \code{as.numeric}, \code{c}, etc..
##' @param a an array
##' @param old.dim the dimensions that are to be restored
##' @param old.dimnames the dimnames that should be  restored
##' @param old.names  the names that should be restored
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
restoredim <- function (a,
                        old.dim = attr (a, "old.dim"),
                        old.dimnames = attr (a, "old.dimnames"),
                        old.names = attr (a, "old.names")){
   dim (a) <- old.dim
   dimnames (a) <- old.dimnames
   names (a) <- old.names
   attr (a, "old.dim") <- NULL
   attr (a, "old.dimnames") <- NULL
   attr (a, "old.names") <- NULL

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
}

##' Little helper functions to work with arrays
##' @name arrayhelpers-package
##' @docType package
##' 
{}

## Installation should be possible without svUnit
if (!require (svUnit)){
  `test<-` <- function (f, value) {
    attr (f, "test") <- value
    f
  }
}

### test data
## vector
v <- 1 : 3
names (v) <- letters [1 : 3]

## matrix
m <- matrix (1:6, 2,
             dimnames = list (rows = letters [1:2],
               columns = LETTERS [1:3])) 

## array
a <- array (1 : 24, 4 : 2,
            dimnames = list (rows = letters [1:4],
              columns = LETTERS [1:3],
              d3 = 1:2)
            )              

##' Run the unit tests
##'
##' Run the unit tests attached to the functions via \link[svUnit]{svUnit} 
##' @return invisibly \code{TRUE} if the tests pass, \code{NA} if \link[svUnit]{svUnit} is not
##' available. Stops if errors are encountered.
##' @author Claudia Beleites
##' @seealso  \link[svUnit]{svUnit} 
##' @export 
arrayhelpers.unittest <- function (){
  if (! require (svUnit)){
    warning ("svUnit required to run the unit tests.")
    return (NA)
  }

  tests <- unlist (eapply (env = getNamespace ("arrayhelpers"), FUN = is.test, all.names = TRUE))
  tests <- names (tests [tests])

  tests <- sapply (tests, get, envir = getNamespace ("arrayhelpers"))

  clearLog ()
  for (t in seq_along (tests))
    runTest (tests [[t]], names (tests) [t])
  print (stats (Log()))

  errorLog (summarize = FALSE)
  invisible (TRUE)
}


.removedim <- function (a){
  dim (a)      <- NULL
  dimnames (a) <- NULL

  a
}

.appenddimafter <- function (a, N, d, dn){
  if (is.null (d)){   # vector
    d <- length (a)
    dn <- list (names (a))
  }
   
  dim (a)        <- c (d,  rep (1,       N - length (d)))
  if (! is.null (dn))
    dimnames (a) <- c (dn, rep (list (), N - length (dn)))
  
  a
}

.collapsedimafter <- function (a, N, d, dn) {
  dim (a)      <- c (d  [seq_len (N - 1)], prod (d [N : length (d)]))
  if (! is.null (dn))
    dimnames (a) <- c (dn [seq_len (N - 1)], list ())

  a  
}

## TODO: append/collapse before


##' Ensure/collapse an array into \code{n} dimensions and restore the old dimensions
##'
##' \code{nameNd} ensures a given number of dimensions:
##' If \code{a} has less than \code{N} dimensions, new dimensions of length 1 are appended.
##' If \code{a} has more than \code{N} dimensions, the supernumerary dimensions are collapsed onto
##' the last dimension.
##'
##' @param a an array (matrix, vector)
##' @param N the desired number of dimensions, 0 to remove the \code{dim} and \code{dimnames}
##' attributes (i.e. to create a vector). 
##' @return N-dimensional array
##' @author Claudia Beleites
##' @export  
##' @examples
##' v <- 1 : 3
##' v
##' makeNd (v, 1L)
##' dim (makeNd (v, 1L))
##' dim (makeNd (v, 3L))
##' 
##' m <- matrix (1:6, 2)
##' m
##' makeNd (m, 1L)
##' dim (makeNd (m, 1L))
##' makeNd (m, 0L) 
##' dim (makeNd (m, 0L))
##' makeNd (m, 3L)
##' 
##' a <- array (1 : 24, 4 : 3)
##' a
##' dim (makeNd (a, 1L))
##' dim (makeNd (a, 0L))
##' makeNd (a, 2L)                          # note the row names
##' 
makeNd <- function (a, N) {
   stopifnot (N >= 0L)
   if (! all.equal (N, as.integer (N)))
     warning ("N truncated to integer value")
   N <- as.integer (N)

   d  <- dim (a)
   dn <- dimnames (a)

   ## store the old dimensions in attributes
   attr (a, "old.names") <- names (a)
   attr (a, "old.dimnames") <- dn
   attr (a, "old.dim")      <- d
 
   if      (N == 0L)        a <- .removedim   (a)
   else if (length (d) < N && N > 0L) a <- .appenddimafter   (a, N, d, dn)
   else if (length (d) > N && N > 0L) a <- .collapsedimafter (a, N, d, dn)

   a
}

test (makeNd) <- function (){
  checkException (makeNd (v, -1))

  checkEqualsNumeric (makeNd (v, 1L), v)
  checkEqualsNumeric (makeNd (v, 2L), v)
  checkEquals (dim (makeNd (v, 1L)),    3L)
  checkEquals (dim (makeNd (v, 2L)), c (3L, 1L))
  checkEquals (dimnames (makeNd (v, 1L)) [[1]], names (v))

  checkEqualsNumeric (makeNd (m, 3L), m)
  checkEquals (dim (makeNd (m, 3L)), c(2L, 3L, 1L))
  checkEquals (dimnames (makeNd (m, 3L)), c(dimnames (m), list (NULL)))
  
  checkEqualsNumeric (makeNd (a, 0L), a)
  checkTrue (is.null (dim (makeNd (a, 0L))))
  checkEquals (dim (makeNd (a, 2L)), c (4L, 6L))
  checkEquals (dimnames (makeNd (a, 2L)), list (rows = letters [1 : 4], NULL))
}
  
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
test (restoredim) <- function (){
  checkIdentical (a, restoredim (makeNd (a, 5)))
  checkIdentical (a, restoredim (makeNd (a, 0)))

  checkIdentical (v, restoredim (makeNd (v, 0)))
  checkIdentical (v, restoredim (makeNd (v, 1)))
  
  checkIdentical (dim (restoredim (as.numeric (makeNd (a, 0L)))), NULL)
}

##' Transpose arrays
##'
##' This function provides transposing of arrays or vectors as swapping their first two dimensions.
##' @param x an array 
##' @return the array with the first two dimensions swapped.
##' @author Claudia Beleites
##' @seealso \code{\link[base]{t}}
##' @export 
##' @examples
##' a <- array (1 : 24, 4:2)
##' a
##' ta (a)
ta <- function (x){
  if (! (is.vector (x) || is.matrix (x) || is.array (x)))
      stop ("x must be array, matrix, or vector.")
      
  a <- makeNd (x, 2L)                 # ensure at least 2 dimensions

  d <- seq_along (dim (x))
  d [1 : 2] <- 2 : 1                    # swap first 2 dimensins
  
  aperm (x, d)
}

test (ta) <- function () {
  checkEqualsNumeric (a, ta (ta (a)))
  checkEquals (a, ta (ta (a)))
  checkIdentical (a, ta (ta (a)))

  checkIdentical (ta (m), t (m))
}



install_ta <- function (){
  setMethod ("t", "array", ta)
}
uninstall_ta <- function (){
  removeMethod ("t", "array")
}

test (install_ta) <- function (){
  ## TODO: make sure it is this package's array method!
  instd <- length (findMethod ("t", "array")) > 0L

  if (instd)
    uninstall_ta ()
  checkException (t (a))
  install_ta ()
  checkIdentical (t (t (a)), a)
  
  if (!instd)
    uninstall_ta ()
}

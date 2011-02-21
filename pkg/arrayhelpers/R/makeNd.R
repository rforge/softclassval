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
##' v <- arrayhelpers:::v
##' v
##' makeNd (v, 1L)
##' dim (makeNd (v, 1L))
##' dim (makeNd (v, 3L))
##' 
##' m <- arrayhelpers:::m
##' m
##' makeNd (m, 1L)
##' dim (makeNd (m, 1L))
##' makeNd (m, 0L) 
##' dim (makeNd (m, 0L))
##' makeNd (m, 3L)
##' 
##' a <- arrayhelpers:::a
##' a
##' dim (makeNd (a, 1L))
##' dim (makeNd (a, 0L))
##' makeNd (a,  2L)          
##' makeNd (a, -2L)
##' makeNd (a, -4L)
##' makeNd (a, 3L)
##' 
makeNd <- function (a, N) {
   if (! all.equal (N, as.integer (N)))
     warning ("N truncated to integer value")
   N <- as.integer (N)

   d  <- dim (a)
   dn <- dimnames (a)

   ## push the old dimensions to the end of attribute old
   push (a, "old") <- list (list (names = names (a), dimnames = dn, dim = d))
 
   if      (N == 0L)        a <- .removedim   (a)
   else if (length (d) <  N && N > 0L) a <- .appenddimafter    (a,  N, d, dn)
   else if (length (d) >  N && N > 0L) a <- .collapsedimafter  (a,  N, d, dn)
   else if (length (d) < -N && N < 0L) a <- .appenddimbefore   (a, -N, d, dn)
   else if (length (d) > -N && N < 0L) a <- .collapsedimbefore (a, -N, d, dn)
    
   a
}

.test (makeNd) <- function (){
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
   
  dim (a)        <- c (d,  rep (1L,          N - length (d)))
  if (! is.null (dn))
    dimnames (a) <- c (dn, rep (list (NULL), N - length (dn)))
  
  a
}
.appenddimbefore <- function (a, N, d, dn){
  if (is.null (d)){   # vector
    d <- length (a)
    dn <- list (names (a))
  }
   
  dim (a)        <- c (rep (1L,          N - length (d )),  d)
  if (! is.null (dn))
    dimnames (a) <- c (rep (list (NULL), N - length (dn)), dn)
  
  a
}

.collapsedimafter <- function (a, N, d, dn) {
  dim (a)        <- c (d  [seq_len (N - 1L)], prod (d [N : length (d)]))
  if (! is.null (dn))
    dimnames (a) <- c (dn [seq_len (N - 1L)], list (NULL))

  a  
}

.collapsedimbefore <- function (a, N, d, dn) {
  dim (a)        <- c (prod (d [seq_len (length (d) - N + 1)]), tail (d,  N - 1L))
  if (! is.null (dn))
    dimnames (a) <- c (list (NULL),                             tail (dn, N - 1L))

  a  
}

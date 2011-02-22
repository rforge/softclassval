##' number of dimensions
##'
##' @param a vector, matrix, or array
##' @return integer: length of dim attribute
##' @author Claudia Beleites
##' @export 
ndim <- function (a){
  length (dim (a))
}

.test (ndim) <- function (){
  checkEquals (ndim (v), 0L)
  checkEquals (ndim (ensuredim (v)), 1L)
  checkEquals (ndim (m), 2L)
  checkEquals (ndim (a), 3L)
}

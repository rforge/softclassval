##' Ensure dim attribute
##'
##' Turns vectors into 1d-arrays, and leaves arrays unchanged.
##' @param v vector (or array)
##' @return vectors are turned in "arrays" of one dimension
##' @author Claudia Beleites
##' @export 
ensuredim <- function (v){
  if (is.null (dim (v))){
    dim (v) <- length (v)
    dimnames (v) <- names (v)
  }

  v
}

.test (ensuredim) <- function (){
  checkEquals (ensuredim (v), structure(1:3, .Dim = 3L))
  checkEquals (ensuredim (m), m)
  checkEquals (ensuredim (a), a)
}

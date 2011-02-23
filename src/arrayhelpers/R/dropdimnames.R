##' Drop dimnames if all elements are \code{NULL}
##'
##' @param x object
##' @return object without empty dimnames
##' @author Claudia Beleites
##' @export 
dropdimnames <- function (x){
  dimnames (x) <- listornull (dimnames (x))

  x
}
##' @rdname dropdimnames
##' @param l list
##' @return \code{listornull}: \code{NULL} if all elements of \code{dn} are \code{NULL}, otherwise \code{dn}
##' @export 
listornull <- function (l){
  if (all (sapply (l, is.null)))
    NULL
  else
    l
}

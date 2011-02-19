##' Attach postprocessing function to operator
##'
##' The postprocessing function is applied during performance calculation after averaging but before
##' \code{\link{dev}} is applied. This is the place where the root is taken of root mean squared errors.
##'
##' \code{postproc (op)} retrieves the postprocessing function (or \code{NULL} if none is attached)
##' 
##' @param op the operator (function)
##' @return logical indicating the type of operator. \code{NA} if the attribute is missing.
##' @author Claudia Beleites
##' @references \Sexpr{cite ("softperformance")}
##' @seealso \code{\link{sens}} \code{\link{post}}
##' @export 
##'
##' @examples
##'
##' postproc (wRMSE)
##' myop <- function (r, p) p * (r == 1)
##' postproc (myop) <- `sqrt`
##' 

postproc <- function (op)
  attr (op, "postproc")

##' @rdname postproc.Rd
##' @param value function (or its name or symbol) to do the post-processing. \code{NULL} deletes the
##' postprocessing function.
`postproc<-` <- function (op, value){
  if (! is.null (value))
    stopifnot (is.function (match.fun (value)))

  attr (op, "postproc") <- value

  op
}

.test (postproc) <- function (){
  myop <- function (){}
  checkTrue (is.null (postproc (myop)))
  postproc (myop) <- `sqrt`
  checkTrue (is.function (match.fun (postproc (myop))))
}

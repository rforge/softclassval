##' Convert character or logical indices to numeric
##' @param x the object that is to be indexed
##' @param n names of the object
##' @param indices the indices to be converted
##' @return numeric indices
##' @author Claudia Beleites
##' @export 
numericindex <- function (x, n = names (x), indices){
  if (is.character (indices))
    match (indices, n)
  else if (is.logical (indices))
    which (indices)
  else if (is.numeric (indices))
    indices
  else
    stop ("indices must be numeric, logical, or character")
}
.test (numericindex) <- function (){
  checkEquals (numericindex (a, names (dimnames (a)), c("rows", "d3", "d4")), c (1L, 3L, NA))
  checkEquals (numericindex (a, names (dimnames (a)), c(TRUE, FALSE, TRUE)), c(1L, 3L))
  checkEquals (numericindex (a, names (dimnames (a)), 1L), 1L)
}

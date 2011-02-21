
##' Count equal rows
##'
##' for data.frames and matrices
##' @param x the matrix or data.frame
##' @return data frame with unique rows and their counts
##' @author Claudia Beleites
##' 
count.rows <- function(x) { 
  if (is.matrix (x) && (dim (x) [2] == 1))
    x <- as.vector (x) 

  order.x <- do.call(order,as.data.frame(x))
  if (is.vector (x)) {
    equal.to.previous <-
      x[tail(order.x,-1)] == x[head(order.x,-1)]
  } else {
    equal.to.previous <-
      rowSums(x[tail(order.x,-1),] != x[head(order.x,-1),])==0
  }

  indices <- split (order.x, cumsum (c (TRUE, !equal.to.previous)))

  if (is.vector (x)) {
    x <- x [sapply (indices, function (v) v [[1]]),   drop = FALSE]
  } else {
    x <- x [sapply (indices, function (v) v [[1]]), , drop = FALSE]
  }

  data.frame (counts = sapply (indices, length),
              ind    = I (indices),
              x)
}

##' Convert to hard class labels
##'
##' Converts the soft class labels in \code{x} into a factor with hard class memberships and
##' \code{NA} for soft samples. 
##' @param x matrix or array holding the class memberships
##' @param classdim 
##' @param soft.name level for soft samples 
##' @param tol tolerance: samples with membership >= 1 - tol are considered to be hard samples of the
##' respective class.
##' @param drop see \code{\link[arrayhelpers]{drop1d}}
##' @param sampledim the dimension that contains the samples
##' @return factor array of shape \code{dim (x) [-classdim]}
##' @author Claudia Beleites
##' @export 
##' @callgraph 
hardclasses <- function (x, classdim = 2L, soft.name = NA, tol = 1e-5, drop = TRUE){
  if (ndim (x) == 0L) {                 # vector
    warning ("Using hardclasses (cbind (x, 1 - x)) instead.")
    x <- cbind (x, 1 - x)
    colnames (x) <- 1 : 0
  }
  
  classdim <- numericindex (x = dim (x), i = classdim, n = names (dimnames (x)))
  x <- aperm (x, c(seq_len (ndim (x))[-classdim], classdim))
  x <- makeNd (x, -2L)
  olddims <- attr (x, "old")[[1L]]
  
  if (any (abs(1 - rowSums (x)) > tol))
    warning ("Found samples with total membership != 1")
  
  classes <- colnames (x)
  x <- x >= 1 - tol                     # looses attributes!
  cl <- apply (x, 1, function (x) match (TRUE, x))
  if (! is.na (soft.name)){
    classes <- c (classes, soft.name)
    cl [is.na (cl)] <- length (classes)
  }

  cl <- structure (cl,
                   .Label = classes, , class = "factor",
                   .Dim = head (olddims$dim, -1L),
                   .Dimnames = listornull (head (olddims$dimnames, -1L)))

  drop1d (cl, drop = drop)
}

.test (hardclasses) <- function (){
  checkEquals (hardclasses (pred),
               factor (rep (letters [c (1, 2, NA, NA, NA)], 2), levels = letters [1 : 3]))

  checkEquals (hardclasses (pred, drop = FALSE), ensuredim (hardclasses (pred)))
}

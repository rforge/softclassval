.rowsum <- function(x, group, reorder=TRUE, na.rm = FALSE, ...) {

  x <- makeNd (x, 2)
  old <- attributes (x)

  x <- base:::rowsum (x, group = group, reorder = reorder, na.rm = na.rm)

  old$old [[1]]$dim [1] <- nrow (x)
  old$old [[1]]$dimnames [1] <- list (rownames (x))

  mostattributes (x) <- old

  x <- restoredim (x)

  x
}

.test (.rowsum) <- function (){
  rowsum (v, seq_along (v))
}

##' rowsum for arrays
##'
##' This function extends the base function \code{\link[base]{rowsum}}.
##' @usage \S4method{rowsum}{array}(x, group, reorder=TRUE, na.rm = FALSE, ...) 
##' @param x array to be \code{rowsum}med
##' @param group grouping variable (integer or factor) indicating groups of samples. \code{}
##' @param reorder should the groups be ordered? see \code{\link[base]{rowsum}}
##' @param na.rm shoud \code{NA}s be removed?
##' @param ... ignored
##' @return like \code{\link[base]{rowsum}}, but further dimensions of the array are preserved.
##' @author Claudia Beleites
##' @seealso \code{\link[base]{rowsum}}
##' @keywords array algebra arith
##' @docType methods
##' @export
setMethod ("rowsum", signature = c (x = "array"), .rowsum)


##' \code{groupsum} extends \code{rowsum}: it allows \code{group} to be an array of the same shape
##' as \code{x}. 
##' @rdname rowsum
##' @export
groupsum <- function(x, group = NULL, dim = 1L, reorder=TRUE, na.rm = FALSE, ...) {
  x <- ensuredim (x)

  ## permute the group dimension to the beginning
  x <- aperm (x, c (dim, seq_len (ndim (x)) [-dim]))

  x <- makeNd (x, 2)
  old <- attributes (x)

  if (is.null (group)){                 # almost no gain...
    x   <- colSums (x, na.rm = TRUE, drop = FALSE)
  } else if (length (group) == nrow (x)) {
    x   <- rowsum  (x, group = group, na.rm = TRUE)
  } else if (all (dim (group) == dim (x))) {
    stop ("grouping factors of size dim (p) not yet implemented.")
  }

  old$old [[1]]$dim [1] <- nrow (x)
  old$old [[1]]$dimnames [1] <- list (rownames (x))

  mostattributes (x) <- old

  x <- restoredim (x)

  aperm (x, order (c (dim, seq_len (ndim (x)) [-dim])))
}
##' @rdname rowsum
##' @export
setGeneric ("groupsum")

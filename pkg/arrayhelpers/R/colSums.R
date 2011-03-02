#  From base/R/colSums.R
.colSums <- function (x, na.rm = FALSE, dims = 1L, drop = TRUE) {
  if (length (dim (x)) < 2)
    x <- as.matrix (x)
  d  <- dim (x)

  if (dims < 1 || dims > length (d) - 1) stop("invalid 'dims'")

  nrow <- prod (head (d,  dims))
  ncol <- prod (tail (d, -dims))
  
  z <- if (is.complex (x))
           .Internal (colSums (Re (x), nrow, ncol, na.rm)) +
      1i * .Internal (colSums (Im (x), nrow, ncol, na.rm))
  else     .Internal (colSums (    x,  nrow, ncol, na.rm))

  d  <- tail (d,            -dims)
  dn <- tail (dimnames (x), -dims)
  if (drop){
    z <- structure (z, .Dim      =      d,
                       .Dimnames = lon (dn))
    z <- drop1d (z)
  } else {                              # ! drop
    z <- structure (z,
                    .Dim      =      c (rep (1,           dims), d),
                    .Dimnames = lon (c (rep (list (NULL), dims), dn)))
  }

  z
}

.unclasscolSums <- function (x, ...) {
  colSums (unclass (x), ...)
}

test (.colSums) <- function (){
  ao <- array (1:24, 4:2)
  
  for (d in 1 : 2){
    default <- base::colSums (a, dims = d)
    drop <- colSums (a, dims = d, drop = TRUE)
    nodrop <- colSums (a, dims = d, drop = FALSE)

    checkEquals (default, drop, sprintf ("base version ./. drop = TRUE, dim = %i", d))
    checkEqualsNumeric (c (default), c (nodrop), sprintf ("drop = TRUE ./. FALSE, dim = %i", d))

    dd <- dim (default)
    if (is.null (dd)) dd <- length (default)
    checkEquals (dim (nodrop) [-(1 : d)], dd, sprintf ("result dimensions, d = %i", d))
    checkTrue (all (sapply (dimnames (nodrop) [1 : d], is.null)))
    checkEquals (dimnames (nodrop) [(d + 1) : ndim (nodrop)],
                 dimnames (a)      [(d + 1) : ndim (a)     ])
    nodrop <- colSums (ao, dims = d, drop = FALSE)
    checkEquals (dimnames (nodrop) [(d + 1) : ndim (nodrop)],
                 dimnames (ao)     [(d + 1) : ndim (ao)    ])
  }
}

## TODO: Rest wie colSums
## TODO: Tests for AsIs, matrix
.colMeans <- function(x, na.rm = FALSE, dims = 1L, drop = TRUE)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    n <- prod(dn[1L:dims])
    dn [1L : dims] <- 1L
    z <- if(is.complex(x))
        .Internal(colMeans(Re(x), n, prod(dn), na.rm)) +
            1i * .Internal(colMeans(Im(x), n, prod(dn), na.rm))
    else .Internal(colMeans(x, n, prod(dn), na.rm))

    if (drop){
      if(length(dn) > dims + 1L) {
        dim(z) <- dn[-(1L:dims)]   
        dimnames(z) <- dimnames(x)[-(1L:dims)]
      } else names(z) <- dimnames(x)[[dims+1]]
    } else {
      dim(z) <- dn
      dimnames(z) <- dimnames(x)
    }
    z
}
.unclasscolMeans <- function (x, ...) {
  colMeans (unclass (x), ...)
}

.rowSums <- function(x, na.rm = FALSE, dims = 1L, drop = TRUE)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    p <- prod(dn[-(1L:dims)])

    dn [(dims + 1L) : length (dn)] <- 1L

    z <- if(is.complex(x))
        .Internal(rowSums(Re(x), prod(dn), p, na.rm)) +
            1i * .Internal(rowSums(Im(x), prod(dn), p, na.rm))
    else .Internal(rowSums(x, prod(dn), p, na.rm))

    if (drop){
      if(dims > 1L) {
        dim(z) <- dn [1L:dims]
        dimnames(z) <- dimnames(x)[1L:dims]
    } else  names(z) <- dimnames(x)[[1L]]
    } else {
      dim(z) <- dn
      dimnames(z) <- dimnames(x)
    }
    z
}
.unclassrowSums <- function (x, ...) {
  rowSums (unclass (x), ...)
}

.rowMeans <- function(x, na.rm = FALSE, dims = 1L, drop = TRUE)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    p <- prod(dn[-(1L:dims)])

    dn [(dims + 1L) : length (dn)] <- 1L

    z <- if(is.complex(x))
        .Internal(rowMeans(Re(x), prod(dn), p, na.rm)) +
            1i * .Internal(rowMeans(Im(x), prod(dn), p, na.rm))
    else .Internal(rowMeans(x, prod(dn), p, na.rm))

    if (drop){
      if(dims > 1L) {
        dim(z) <- dn [1L:dims]
        dimnames(z) <- dimnames(x)[1L:dims]
    } else  names(z) <- dimnames(x)[[1L]]
    } else {
      dim(z) <- dn
      dimnames(z) <- dimnames(x)
    }
    z
}
.unclassrowMeans <- function (x, ...) {
  rowMeans (unclass (x), ...)
}

test (.rowSums) <- function (){
  a <- array (1:24, 4:2)
  for (d in 1 : 2){
    default <- base::rowSums (a, dims = d)
    drop <- rowSums (a, dims = d, drop = TRUE)
    nodrop <- rowSums (a, dims = d, drop = FALSE)

    checkEquals (default, drop, sprintf ("base version ./. drop = TRUE, dim = %i", d))
    checkEquals (c (default), c (nodrop), sprintf ("drop = TRUE ./. FALSE, dim = %i", d))

    dd <- dim (default)
    if (is.null (dd)) dd <- length (default)
    checkEquals (dim (nodrop) [1 : d], dd, sprintf ("result dimensions, d = %i", d))
  }
}

test (.rowMeans) <- function (){
  a <- array (1:24, 4:2)
  for (d in 1 : 2){
    default <- base::rowMeans (a, dims = d)
    drop <- rowMeans (a, dims = d, drop = TRUE)
    nodrop <- rowMeans (a, dims = d, drop = FALSE)

    checkEquals (default, drop, sprintf ("base version ./. drop = TRUE, dim = %i", d))
    checkEquals (c (default), c (nodrop), sprintf ("drop = TRUE ./. FALSE, dim = %i", d))

    dd <- dim (default)
    if (is.null (dd)) dd <- length (default)
    checkEquals (dim (nodrop) [1 : d], dd, sprintf ("result dimensions, d = %i", d))
  }
}

test (.colMeans) <- function (){
  a <- array (1:24, 4:2)
  for (d in 1 : 2){
    default <- base::colMeans (a, dims = d)
    drop <- colMeans (a, dims = d, drop = TRUE)
    nodrop <- colMeans (a, dims = d, drop = FALSE)

    checkEquals (default, drop, sprintf ("base version ./. drop = TRUE, dim = %i", d))
    checkEquals (c (default), c (nodrop), sprintf ("drop = TRUE ./. FALSE, dim = %i", d))

    dd <- dim (default)
    if (is.null (dd)) dd <- length (default)
    checkEquals (dim (nodrop) [-(1L : d)], dd, sprintf ("result dimensions, d = %i", d))
  }
}

# ##' @nord
setGeneric ("colSums")
##' @nord
setGeneric ("colMeans")
##' @nord
setGeneric ("rowSums")
##' @nord
setGeneric ("rowMeans")

##' Row and column sums and means for numeric arrays. 
##'
##' These functions extend the respective base functions by (optionally) preserving the shape of the
##' array (i.e. the summed dimensions have length 1).
##' 
##' @param x an array of two or more dimensions, containing numeric, complex, integer or logical
##' values, or a numeric data frame.  
##' @param na.rm logical indicating treatment of missing values
##' @param dims integer: Which dimensions are regarded as \sQuote{rows} or \sQuote{columns} to sum
##' over.  For \code{row*}, the sum or mean is  over dimensions \code{dims + 1, \dots}; for \code{col*}
##' it is over  dimensions \code{1 : dims}.
##' @param ... the \code{signature = "AsIs"} methods hand on all parameters
##' @param drop If \code{FALSE}, the number of dimensions is retained: the length of the dimensions
##' that are summed or averaged is set to  1. \code{TRUE} yield the same behaviour as
##' \code{\link[base]{colSums}}
##' @return like \code{\link[base]{colSums}} if \code{drop = TRUE}, otherwise an array where the
##' summed dimensions have length 1.
##' @author Claudia Beleites
##' @seealso \code{\link[base]{colSums}}
##' @keywords array algebra arith
##' @docType methods
##' @rdname colSums
##' @export
##'
##' @examples
##' a <- array (1 : 24, 4 : 2)
##' a
##' 
##' rowSums (a)
##' rowSums (a, drop = FALSE)
##' 
##' colSums (a)
##' colSums (a, drop = FALSE)
##' 
##' colSums (a, dim = 2)
##' colSums (a, dim = 2, drop = FALSE)
##'
setMethod ("colSums", signature = c ("matrix"), .colSums) 
# colSums.matrix <- .colSums              # I still get base::colSums :-(

##' @rdname colSums
##' @export
colSums.AsIs <- .unclasscolSums
# setMethod ("colSums", signature = c (x = "AsIs"), .unclasscolSums)

##' @rdname colSums
##' @export
setMethod ("colSums", signature = c (x = "array"), .colSums)

##' @rdname colSums
##' @export
setMethod ("colMeans", signature = c (x = "matrix"), .colMeans)

##' @rdname colSums
##' @export
colMeans.AsIs <- .unclasscolMeans
##setMethod ("colMeans", signature = c (x = "AsIs"), .unclasscolMeans)

##' @rdname colSums
##' @export
setMethod ("colMeans", signature = c (x = "array"), .colMeans)

##' @rdname colSums
##' @export
setMethod ("rowSums", signature = c (x = "matrix"), .rowSums)

##' @rdname colSums
##' @export
rowSums.AsIs <- .unclassrowSums
#setMethod ("rowSums", signature = c (x = "AsIs"), .unclassrowSums)

##' @rdname colSums
##' @export
setMethod ("rowSums", signature = c (x = "array"), .rowSums)

##' @rdname colSums
##' @export
setMethod ("rowMeans", signature = c (x = "matrix"), .rowMeans)

##' @rdname colSums
##' @export
rowMeans.AsIs <- .unclassrowMeans
##setMethod ("rowMeans", signature = c (x = "AsIs"), .unclassrowMeans)

##' @rdname colSums
##' @export
setMethod ("rowMeans", signature = c (x = "array"), .rowMeans)

testAsIs <- function (){
  methods <- c("colSums", "colMeans", "rowSums", "rowMeans")
  for (fn in methods){
    f <- get (fn)
    for (d in 1L : 2L)
      checkEquals (f (a, dims = d), f (I (a), dims = d), msg = sprintf ("AsIs: %s, dims = %i", fn, d))
  }
}

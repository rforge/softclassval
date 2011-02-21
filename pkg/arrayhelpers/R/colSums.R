#  From base/R/colSums.R
.colSums <- function(x, na.rm = FALSE, dims = 1L, drop = TRUE)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    n <- prod(dn[1L:dims])

    dn [1L : dims] <- 1L
    
    z <- if(is.complex(x))
        .Internal(colSums(Re(x), n, prod(dn), na.rm)) +
            1i * .Internal(colSums(Im(x), n, prod(dn), na.rm))
    else .Internal(colSums(x, n, prod(dn), na.rm))

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

test (.colSums) <- function (){
  a <- array (1:24, 4:2)
  for (d in 1 : 2){
    default <- base::colSums (a, dims = d)
    drop <- colSums (a, dims = d, drop = TRUE)
    nodrop <- colSums (a, dims = d, drop = FALSE)

    checkEquals (default, drop, sprintf ("base version ./. drop = TRUE, dim = %i", d))
    checkEquals (c (default), c (nodrop), sprintf ("drop = TRUE ./. FALSE, dim = %i", d))

    dd <- dim (default)
    if (is.null (dd)) dd <- length (default)
    checkEquals (dim (nodrop) [-(1L : d)], dd, sprintf ("result dimensions, d = %i", d))
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
##' @param ... ignored
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
#colSums.matrix <- .colSums
#colSums.array <- .colSums
setMethod ("colSums", signature = c ("matrix"), .colSums)

##' @rdname colSums
##' @export
setMethod ("colSums", signature = c (x = "array"), .colSums)

##' @rdname colSums
##' @export
setMethod ("colMeans", signature = c (x = "matrix"), .colMeans)

##' @rdname colSums
##' @export
setMethod ("colMeans", signature = c (x = "array"), .colMeans)

##' @rdname colSums
##' @export
setMethod ("rowSums", signature = c (x = "matrix"), .rowSums)

##' @rdname colSums
##' @export
setMethod ("rowSums", signature = c (x = "array"), .rowSums)

##' @rdname colSums
##' @export
setMethod ("rowMeans", signature = c (x = "matrix"), .rowMeans)

##' @rdname colSums
##' @export
setMethod ("rowMeans", signature = c (x = "array"), .rowMeans)


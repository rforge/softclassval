##' Input checks for performance calculation
##'
##' Checks whether \code{r} and \code{p} are valid reference and predictions. If \code{p} is a
##' multiple of \code{r}, recycles \code{r} to the size and shape of \code{p}. If \code{r} has
##' additional length 1 dimensions (usually because dimensions were dropped from \code{p}), it is
##' shortend to the shape of \code{p}.
##' @param r reference
##' @param p prediction
##' @return \code{r}, possibly recycled to length of \code{p} or with dimensions shortened to \code{p}.
##' @author Claudia Beleites
##' @nord
.checkrp <- function (r, p){
  if (is.null (dr <- dim (r))) dr <- length (r)
  if (is.null (dp <- dim (p))) dp <- length (p)

  recycle <- prod (dp) > prod (dr)
  
  if (prod (dr) > prod (dp))
    stop ("r must not be larger than p")

  dp <- dp [seq_along (dr)]
  
  if (dr [1] !=  dp [1])       
    stop ("numer of samples (nrow) of reference and prediction must be the same.")
  
  if (! is.na (dr [2]) && dr [2] != dp [2]) 
    stop ("p and r do not have the same number of columns (classes).")

  if (any (is.na (dp)) || any (dr != dp)) { # NA: p is shorter than r
   
    equaldims <- seq_len (min (which (is.na (dp) | dr != dp)) - 1) # first equal dims

    ## thereafter only length 1 dimensions are allowed
    if (any (dr [- equaldims] != 1)) 
      stop ("Dimension mismatch between r and p.")

    ## if p is shorter than r: shorten r
    if (any (is.na (dp))){
      a <- attributes (r)
      a$dim <- a$dim [equaldims]
      a$dimnames <- a$dimnames  [equaldims]
      mostattributes (r) <- a
    }
  }

  if (recycle) {
    a <- attributes (r)
    r <- rep (r, length.out = length (p))
    mostattributes (r) <- attributes (p)
    dimnames (r) [seq_along (a$dimnames)] <- a$dimnames
  }
  
  r
}
test (.checkrp) <- function (){
  checkEquals (.checkrp (ref,       pred                    ), ref      )
  checkEquals (.checkrp (ref.array, pred.array              ), ref.array)
  checkEquals (.checkrp (ref      , pred.array              ), ref.array, msg = "recycling r")
  checkEquals (.checkrp (ref.array [,,1, drop = FALSE], pred), ref      , msg = "shortening r")

  checkException (.checkrp (ref.array, pred                 )           , msg = "length (dim (r)) > length (dim (p))")
  checkException (.checkrp (1 : 2,     1                    )           , msg = "nrow (r) != nrow (p)")
  checkException (.checkrp (ref,       pred [, 1 : 2]       )           , msg = "ncol (r) != ncol (p)")
  
  tmp <- ref.array
  dim (tmp) <- c (dim(ref.array) [1 : 2], 1, dim (ref.array) [3])
  checkException (.checkrp (tmp,       pred.array           )           , msg = "Dimension mismatch")
}

##' Performance calculation for soft classification
##'
##' These performance measures can be used with prediction and reference being continuous class
##' memberships in [0, 1].
##'
##' The rows of \code{r} and \code{p} are considered the samples, columns will usually hold the
##' classes, and further dimensions are preserved but ignored.
##'
##' \code{r} must have the same number of rows and columns as \code{p}, all other dimensions may be
##' filled by recycling.
##'
##' \code{spec}, \code{ppv}, and \code{npv} use the symmetry between the performance measures as
##' described in the article and call \code{sens}.
##'
##' @rdname performance
##' @param r vector, matrix, or array with reference. 
##' @param p vector, matrix, or array with predictions
##' @param groups grouping variable for the averaging by \code{\link[base]{rowsum}}. If \code{NULL},
##' all samples (rows) are averaged.
##' @param operator the \code{\link[softclassval]{operators}} to be used 
##' @param drop should the results possibly be returned as vector instead of 1d array? (Note that
##' levels of \code{groups} are never dropped, you need to do that e.g. by
##' \code{\link[base]{factor}}.)
##' @param .checked for internal use: the inputs are guaranteed to be of same size and shape. If
##' \code{TRUE}, \code{confusion} omits input checking
##' @return numeric of size (ngroups x \code{dim (p) [-1]}) with the respective performance measure
##' @author Claudia Beleites
##' @seealso Performance measures: \code{\link{sens}}
##' @references see the literature in \code{citation ("softclassval")}
##' @export
##' @include softclassval.R
confusion <- function (r = stop ("missing reference"), p = stop ("missing prediction"),
                       groups = NULL,
                       operator = "prd",
                       drop = FALSE, .checked = FALSE){
  operator <- match.fun (operator)
  if (! .checked)
    r <- .checkrp (r, p)
  res <- operator (r = r, p = p)
  res <- groupsum (res, group = groups, dim = 1, reorder = FALSE, na.rm = TRUE)

  drop1d (res, drop = drop)
}
test (confusion) <- function (){
  
}
##TODO tests
##TODO test grouping

##' @param eps limit below which denominator is considered 0
##' @param op.dev does the operator measure deviation?
##' @param op.postproc if a post-processing function is needed after averaging, it can be given
##' here. See the example.
##' @rdname performance
##' @export
sens <- function (r = stop ("missing reference"), p = stop ("missing prediction"), groups = NULL,
                  operator = "prd",
                  op.dev = dev (match.fun (operator)),
                  op.postproc = postproc (match.fun (operator)),
                  eps = 1e-8,
                  drop = FALSE,
                  .checked = FALSE){
  force (op.dev)
  force (op.postproc)

  if (! (isTRUE (op.dev) | isTRUE (! op.dev)))
    stop ("op.dev must either be TRUE or FALSE.")
  
  if (!is.null (op.postproc))
    POSTFUN <- match.fun (op.postproc)

  if (!.checked)
    r <- .checkrp (r, p)                     # do the input checks.
  
  res <- confusion (r = r, p = p, groups = groups, operator = operator, drop = FALSE)
  nsmpl <- nsamples (r = r, groups = groups, operator = operator)

  if (any (nsmpl < res))
    warning ("denominator < enumerator.")
  nsmpl [nsmpl < eps] <- NA

  res <- res / nsmpl
  
  if (! is.null (op.postproc))          # e.g. root for wRMSE
    res <- POSTFUN (res)         
      
  if (op.dev)                           # for wMAE, wMSE, wRMSE, and the like
    res <- 1 - res

  res
}
test (sens) <- function (){
  ops <- c ("luk", "gdl", "prd", "and", "wMAE", "wMSE", "wRMSE")

  ## shape & names
  for (o in ops){
    ## vector
    tmp <- sens (r = v, p = v, operator = o)
    checkEquals (dim (tmp), 1L)
    checkTrue (is.null (dimnames (tmp))[[1]])
    checkTrue (is.null (names (tmp)))

    ## matrix
    tmp <- sens (r = v [1 : 4], p = m, operator = o)
    checkEquals (dim (tmp), c(1L, ncol (m)), msg = "matrix")
    checkEquals (dimnames (tmp), list (NULL, colnames (m)), msg = "matrix")
    checkTrue (is.null (names (tmp)), msg = "matrix")
    
    ## array
    tmp <- sens (r = rep (v [1 : 5], 2), p = pred.array, operator = o)
    checkEquals (dim (tmp), c (1, dim (pred.array) [-1]), msg = "array")
    checkEquals (dimnames (tmp), c (list (NULL), dimnames (pred.array) [-1]), msg = "array")
    checkTrue (is.null (names (tmp)), msg = "array")
  }
  
  checkEqualsNumeric (sens (r = ref.array, p = pred.array),
                      c (0.6, 0.32, NA, 0.85, 0.4, NA))
  
  checkEqualsNumeric (sens (r = ref.array, p = pred.array, operator = "gdl"),
                      c (0.675, 0.5, NA, 1, 1, NA))

  checkEqualsNumeric (sens (r = ref.array, p = pred.array, operator = "luk"),
                      c (0.55, 0.2, NA, 0.75, 0, NA))
  
  checkEqualsNumeric (sens (r = ref.array, p = pred.array, operator = "prd"),
                      c (0.6, 0.32, NA, 0.85, 0.4, NA))
  
  checkEqualsNumeric (sens (r = ref.array, p = pred.array, operator = "and"),
                      c (0.2, NA, NA, 1, NA, NA))
  
  checkEqualsNumeric (sens (r = ref.array, p = pred.array, operator = "wMAE"),
                      c (0.66, 0.68, NA, 1, 1, NA))
  
  checkEqualsNumeric (sens (r = ref.array, p = pred.array, operator = "wMSE"),
                      c (0.788, 0.86, NA, 1, 1, NA))

  checkEqualsNumeric (sens (r = ref.array, p = pred.array, operator = "wRMSE"),
                      c (1 - sqrt (1.696/8), 1 - sqrt (.28/2), NA, 1, 1, NA))


  checkEqualsNumeric (sens (r = ref.array, p = pred.array, groups = ref.groups),
                      c (0.6, 0.6, NA, 0.32, NA, NA, 1, 0.6, NA, 0.4, NA, NA))

  checkEqualsNumeric (sens (r = ref.array, p = pred.array, groups = ref.groups, operator = "gdl"),
                      c (0.6, 0.8, NA, 0.5, NA, NA, 1, 1, NA, 1, NA, NA))

  checkEqualsNumeric (sens (r = ref.array, p = pred.array, groups = ref.groups, operator = "luk"),
                      c (0.6, 1.4/3, NA, 0.2, NA, NA, 1, 1/3, NA, 0, NA, NA))

  checkEqualsNumeric (sens (r = ref.array, p = pred.array, groups = ref.groups, operator = "prd"),
                      c (0.6, 0.6, NA, 0.32, NA, NA, 1, 0.6, NA, 0.4, NA, NA))

  checkEqualsNumeric (sens (r = ref.array, p = pred.array, groups = ref.groups, operator = "and"),
                      c (0.2, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA))

  checkEqualsNumeric (sens (r = ref.array, p = pred.array, groups = ref.groups, operator = "wMAE"),
                      c (0.6, 0.76, NA, 0.68, NA, NA, 1, 1, NA, 1, NA, NA))

  checkEqualsNumeric (sens (r = ref.array, p = pred.array, groups = ref.groups, operator = "wMSE"),
                      c (0.728, 0.888, NA, 0.86, NA, NA, 1, 1, NA, 1, NA, NA))

  checkEqualsNumeric (sens (r = ref.array, p = pred.array, groups = ref.groups, operator = "wRMSE"),
                      1 - sqrt (1 - c (0.728, 0.888, NA, 0.86, NA, NA, 1, 1, NA, 1, NA, NA)))

}

##' @param ... handed to \code{sens}
##' @rdname performance
##' @export 
spec <- function (r = stop ("missing reference"), p = stop ("missing prediction"), ...){
  sens (r = 1 - r, p = 1 - p, ...)
}

##' @rdname performance
##' @export 
ppv <- function (r = stop ("missing reference"), p = stop ("missing prediction"), ...){
  r <- .checkrp (r, p) 
  sens (r = p, p = r, ..., .checked = TRUE)
}
test (ppv) <- function (){
  checkEquals (ppv (r = ref, p = pred.array),
               sens (r = pred.array, p = ref.array))
}

##' @rdname performance
##' @export 
npv <- function (r = stop ("missing reference"), p = stop ("missing prediction"), ...){
  r <- .checkrp (r, p)
  sens (r = 1 - p, p = 1 - r, ..., .checked = TRUE)
}
test (npv) <- function (){
  checkEquals (npv (r = ref, p = pred.array),
               sens (r = 1 - pred.array, p = 1 - ref.array))
}

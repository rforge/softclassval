##' Input checks for performance calculation
##'
##' Checks whether r and p are valid reference and predictions, and recycles r to the size and shape
##' of p
##' @param r reference
##' @param p prediction
##' @return recycled r
##' @author Claudia Beleites
##' @nord
.checkrp <- function (r, p){
  if (is.null (dr <- dim (r))) dr <- length (r)
  if (is.null (dp <- dim (p))) dp <- length (p)

  recycle <- prod (dp) > prod (dr)
  
  dp <- dp [seq_along (dr)]
  
  if (dr [1] !=  dp [1])       # rows = samples: must be the same
    stop ("numer of samples (nrow) of reference and prediction must be the same.")
  
  if (! is.na (dr [2]) && dr [2] != dp [2]) # cols usually = classes, but may be something else: warn only
    warning ("p and r do not have the same number of columns (classes).")
  
  if (length (dr) > length (dp))
    stop ("r must not have more dimensions than p")

  if (any (dr != dp)){
    dr <- dr [-(1 : (min (which (dr != dp)) - 1))] # the first dim with differences
    if (any (dr != 1))                   # thereafter only length 1 dimensions are allowed
      stop ("From the first dimension on where r and p differ in length",
            "r must have length 1 at most.")
  }

  if (recycle) {
    r <- rep (r, length.out = length (p))
    dim (r) <- dim (p)
  }
  r
}

## TODO: test fun

##' Performance calculation for soft classification
##'
##' These performance measures can be used with prediction and reference being continuous class
##' memberships in [0, 1].
##'
##' The rows of \code{r} and \code{p} are considered the samples, columns will usually hold the
##' classes, and further dimensions are preserved but ignored.
##'
##' \code{r} must have the same number of rows as \code{p}, all other dimensions may be filled by
##' recycling.
##'
##' \code{spec}, \code{ppv}, and \code{npv} use the symmetry between the performance measures as
##' described in the article and call \code{sens}.
##'
##' @rdname performance
##' @param r vector, matrix, or array with reference. 
##' @param p vector, matrix, or array with predictions
##' @param group grouping variable for the averaging by \code{\link[base]{rowsum}}. If \code{NULL},
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
                       group = NULL,
                       operator = "prd",
                       drop = FALSE, .checked = FALSE){
  operator <- match.fun (operator)
  if (! .checked)
    r <- .checkrp (r, p)
  res <- operator (r = r, p = p)
  res <- groupsum (res, group = group, dim = 1, reorder = FALSE, na.rm = TRUE)

  drop1d (res, drop = drop)
}

##TODO tests

##' @param eps limit below which denominator is considered 0
##' @param op.dev does the operator measure deviation?
##' @param op.postproc if a post-processing function is needed after averaging, it can be given
##' here. See the example.
##' @rdname performance
##' @export
sens <- function (r = stop ("missing reference"), p = stop ("missing prediction"), group = NULL,
                  operator = "prd",
                  op.dev = dev (match.fun (operator)),
                  op.postproc = postproc (match.fun (operator)),
                  eps = 1e-8,
                  drop = FALSE){
  force (op.dev)
  force (op.postproc)

  if (! (isTRUE (op.dev) | isTRUE (! op.dev)))
    stop ("op.dev must either be TRUE or FALSE.")
  
  if (!is.null (op.postproc))
    POSTFUN <- match.fun (op.postproc)

  r <- .checkrp (r, p)                     # do the input checks.
  
  res <- confusion (r = r, p = p, group = group, operator = operator, drop = FALSE)
  nsmpl <- groupsum (r, group = group, dim = 1, reorder = FALSE, na.rm = TRUE)

  if (any (nsmpl < res))
    warning ("denominator < enumerator.")
  nsmpl [nsmpl < eps] <- NA

  res <- res / nsmpl
  
  if (! is.null (op.postproc))          # the root of the wRMSE
    res <- POSTFUN (res)         
      
  if (op.dev)                           # for wMAE, wMSE, wRMSE, and the like
    res <- 1 - res

  res
}
.test (sens) <- function (){
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
  
  checkEquals (sens(r = ref, p = ref),
               structure (c (0.85, 0.4, NA), .Dim = c(1L, 3L),
                          .Dimnames = list(NULL, c("A", "B", "C"))))
  checkEquals (sens (r = ref, p = ref, group = rep (c ("H", "S"), each = 5)),
               structure(c(1, 0.6, NA, 0.4, NA, NA), .Dim = 2:3,
                         .Dimnames = list(c("H", "S"), c("A", "B", "C"))))

  checkEquals (sens (r = ref, p = ref, operator="gdl"),
               structure(c(1, 1, NA), .Dim = c(1L, 3L),
                         .Dimnames = list(NULL, c("A", "B", "C"))))

  checkEquals (sens (r = ref, p = ref, group = rep (c ("H", "S"), each = 5), operator="gdl"),
               structure (c (1, 1, NA, 1, NA, NA), .Dim = 2:3,
                          .Dimnames = list(c ("H", "S"), c("A", "B", "C"))))


  checkEquals (sens (r = ref, p = ref, operator="luk"),
               structure (c (0.75, 0, NA), .Dim = c(1L, 3L),
                          .Dimnames = list(NULL, c("A", "B", "C"))))
  checkEquals (sens (r = ref, p = ref, group = rep (c ("H", "S"), each = 5), operator="luk"),
               structure (c (1, 0.333333333333333, NA, 0, NA, NA), .Dim = 2:3,
                          .Dimnames = list (c ("H", "S"), c("A", "B", "C"))))

  checkEquals (sens (r = ref, p = ref, operator="wMAE"),
               structure(c(1, 1, NA), .Dim = c(1L, 3L),
                         .Dimnames = list(NULL, c("A", "B", "C"))))
  checkEquals (sens (r = ref, p = ref, group = rep (c ("H", "S"), each = 5), operator="wMAE"),
               structure (c (1, 1, NA, 1, NA, NA), .Dim = 2:3,
                          .Dimnames = list(c ("H", "S"), c("A", "B", "C"))))

  checkEquals (sens (r = ref, p = ref, operator="wMSE"),
               structure(c(1, 1, NA), .Dim = c(1L, 3L),
                         .Dimnames = list(NULL, c("A", "B", "C"))))
  checkEquals (sens (r = ref, p = ref, group = rep (c ("H", "S"), each = 5), operator="wMSE"),
               structure (c (1, 1, NA, 1, NA, NA), .Dim = 2:3,
                          .Dimnames = list(c ("H", "S"), c("A", "B", "C"))))

  checkEquals (sens (r = ref, p = ref, operator="wRMSE"),
               structure(c(1, 1, NA), .Dim = c(1L, 3L),
                         .Dimnames = list(NULL, c("A", "B", "C"))))
  
  checkEqualsNumeric (sens (r = ref, p = ref, group = rep (c ("H", "S"), each = 5),
                            operator="wRMSE"),
                      c (1,  1,
                         NA, 1,
                         NA, NA))


  tmp <- pred
  tmp [which (ref == 0)] <- NA          # which keeps the attributes

            
}

##' @param ... handed to \code{sens}
##' @rdname performance
##' @export 
spec <- function (r = stop ("missing reference"), p = stop ("missing prediction"), ...){
  sens (r = 1 - r, p = 1 - p)
}

##' @rdname performance
##' @export 
ppv <- function (r = stop ("missing reference"), p = stop ("missing prediction"), ...){
  sens (r = p, p = r)
}

##' @rdname performance
##' @export 
npv <- function (r = stop ("missing reference"), p = stop ("missing prediction"), ...){
  sens (r = 1 - p, p = 1 - r)
}

##' Performance calculation for soft classification
##'
##' These performance measures can be used with prediction and reference being continuous class
##' memberships in [0, 1].
##'
##' The rows of \code{r} and \code{p} are considered the samples, columns will usually hold the
##' classes, and further dimensions are preserved but ignored.
##'
##' \code{r} must have the same number of rows as \code{p}, all other dimensions may be filled by recycling. If the c
##'
##' @rdname performance
##' @param r vector, matrix, or array with reference. 
##' @param p vector, matrix, or array with predictions
##' @param group grouping variable for the averaging by \code{\link[base]{rowsum}}. If \code{NULL},
##' all samples (rows) are averaged.
##' @param operator the \code{\link[softperformance]{operators}} to be used 
##' @param eps limit below which denominator is considered 0
##' @param op.dev does the operator measure deviation?
##' @param op.postproc if a post-processing function is needed after averaging, it can be given here. See the example.
##' @return numeric of size (ngroups x \code{dim (p) [-1L]})
##' @author Claudia Beleites
##' @seealso Performance measures: \code{\link{sens}}
##' @references see the literature in \code{citation ("softperformance")}
##' @export
##' @include softperformance.R
sens <- function (r = stop ("missing reference"), p = stop ("missing prediction"),
                  group = NULL,
                  operator = "prd", op.dev = dev (operator), op.postproc = postproc (operator),
                  eps = 1e-8){

  operator <- match.fun (operator)
  force (op.dev)
  force (op.postproc)

  if (! (isTRUE (op.dev) | isTRUE (! op.dev)))
    stop ("op.dev must either be TRUE or FALSE.")
  
  if (!is.null (op.postproc))
    POSTFUN <- match.fun (op.postproc)

  ## check prediction and reference
  r <- ensuredim (r)
  p <- ensuredim (p)
  dr <- dim (r)
  dp <- dim (p)

  rec <- prod (dp [- seq_along (dr)])
  dp <- dp [seq_along (dr)]
  
  stopifnot (dr [1L] ==  dp [1L])       # rows = samples: must be the same
  
  if (! is.na (dr [2L]) && dr [2L] != dp [2L]) # cols usually = classes, but may be something else: warn only
    warning ("p and r do not have the same number of columns.")
  
  if (length (dr) > length (dp))
    stop ("r must not have more dimensions than p")

  if (any (dr != dp)){
    dr <- dr [-(1 : (min (which (dr != dp)) - 1))] # the first dim with differences
    if (any (dr != 1L))                   # thereafter only length 1 dimensions are allowed
      stop ("From the first dimension on where r and p differ in length",
            "r must have length 1 at most.")
  }

  res <- operator (rep (r, rec), p)
  
  if (is.null (group)){                 # almost no gain...
    ## make sure we get the version that allows drop = FALSE
    res   <- arrayhelpers::colSums (res, na.rm = TRUE, drop = FALSE)
    nsmpl <- arrayhelpers::colSums (r  , na.rm = TRUE, drop = FALSE)
  } else {
    res   <- arrayhelpers::rowsum  (res, group = group, na.rm = TRUE)
    nsmpl <- arrayhelpers::rowsum  (r  , group = group, na.rm = TRUE)
  }
  nsmpl <- rep (nsmpl, rec)

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
    checkEquals (dim (), 1L)
    checkTrue (is.null (dimnames (tmp))[[1L]])
    checkTrue (is.null (names (tmp)))

    ## matrix
    tmp <- sens (r = v [1 : 4], p = m, operator = o)
    checkEquals (dim (tmp), c(1L, ncol (m)))
    checkEquals (dimnames (tmp), list (NULL, colnames (m)))
    checkTrue (is.null (names (tmp)))
    
    
    ## array
    tmp <- sens (r = rep (v [1 : 5], 2), p = pred.array, operator = o)
    checkEquals (dim (tmp), c(1L, ncol (m)))
    checkEquals (dimnames (tmp), list (NULL, colnames (m)))
    checkTrue (is.null (names (tmp)))
    
    
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
  
  checkEqualsNumeric (sens (r = ref, p = ref, group = rep (c ("H", "S"), each = 5), operator="wRMSE"),
                      c (1,  1,
                         NA, 1,
                         NA, NA))


  tmp <- pred
  tmp [which (ref == 0)] <- NA          # which keeps the attributes
  
  checkEqualsNumeric (sens (r = ref, p = p, operator="prd", group = 1:10), tmp) 
  sens (r = ref, p = p, operator="prd", group = rep (1L : 2, each = 5))
  sens (r = ref, p = p, operator="prd", group = rep (1L : 5, 2))             
}


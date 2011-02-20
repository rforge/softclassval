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
##' @param ... ensures that arguments are named
##' @param r vector, matrix, or array with reference. 
##' @param p vector, matrix, or array with predictions
##' @param group grouping variable for the averaging by \code{\link[base]{rowsum}}. If \code{NULL},
##' all samples (rows) are averaged.
##' @param operator the \code{\link[softperformance]{operators}} to be used 
##' @param dev does the operator measure deviation?
##' @param postproc if a post-processing function is needed after averaging, it can be given here. See the example.
##' @param eps limit below which denominator is considered 0
##' @return numeric of size (ngroups x \code{dim (p) [-1L]})
##' @author Claudia Beleites
##' @seealso Performance measures: \code{\link{sens}}
##' @references see the literature in \code{citation ("softperformance")}
##' @export 
##' @include softperformance.R
sens <- function (..., r, p, group = NULL,
                  operator = "prd", op.dev = dev (operator), op.postproc = postproc (operator),
                  eps = 1e-8){
  ## make sure the arguments are correctly named
  dots <- list (...)
  if (length (dots) > 0L)
    stop ("Unknown arguments:", names (dots))
  
  operator <- match.fun (operator)
  force (op.dev)
  force (op.postproc)

  if (! (isTRUE (op.dev) | isTRUE (! op.dev)))
    stop ("op.dev must either be TRUE or FALSE.")
  
  if (!is.null (op.postproc))
    POSTFUN <- match.fun (op.postproc)

  ## check prediction and reference
  dr <- dim (r);  if (is.null (dr)) dr <- length (r)
  dp <- dim (p);  if (is.null (dp)) dp <- length (p)
  
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
  
  p <- makeNd (p, 2L)                   
  r <- makeNd (p, 2L)
  mostattributes (r) <- attributes (p)  # make sure the attributes come from p: r may be the same for
                                        # all columns and further dimensions

  ## here's the real calculation

  res <- operator (r, p)

  if (is.null (group)){                 # almost no gain...
    res  <- colSums (res, na.rm = TRUE)
    nsmpl <- colSums (r   , na.rm = TRUE)
  } else {
    res  <- rowsum  (res, group = group, na.rm = TRUE)
    nsmpl <- rowsum  (r   , group = group, na.rm = TRUE)
  }

  if (any (nsmpl < res))
    warning ("denominator < enumerator.")
  nsmpl [nsmpl < eps] <- NA

  res <- res / nsmpl
  
  if (! is.null (op.postproc))          # the root of the wRMSE
    res <- POSTFUN (res)         
      
  if (op.dev)                           # for wMAE, wMSE, wRMSE, and the like
    res <- 1 - res

#  restoredim (res,
#              old.dim = )
  res
}


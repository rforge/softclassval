##' @encoding UTF-8
##' @description And (conjunction) operators
##'
##' And operators for the soft performance calculation.
##'
##' The predefined operators are:
##' \tabular{lllll}{
##' Name         \tab Definition                 \tab \code{\link{dev}}? \tab \code{\link{postproc}}? \tab Explanation                                                  \cr
##' \code{gdl}   \tab \code{pmin (r, p)}         \tab FALSE              \tab FALSE                   \tab the \enc{Gödel}{Goedel}-operator (weak conjunction)          \cr
##' \code{luk}   \tab \code{pmax (r + p - 1, 0)} \tab FALSE              \tab FALSE                   \tab \enc{Łukasiewicz}{Lukasiewicz}-operator (strong conjunction) \cr
##' \code{prd}   \tab \code{r * p}               \tab FALSE              \tab FALSE                   \tab product operator                                             \cr
##' \code{and}   \tab \code{r * p}               \tab FALSE              \tab FALSE                   \tab Boolean conjunction: accepts only 0 or 1, otherwise yields \code{NA}  \cr
##' \code{wMAE}  \tab \code{r * abs (r - p)}     \tab TRUE               \tab FALSE                   \tab for weighted mean absolute error                                 \cr
##' \code{wMSE}  \tab \code{r * (r - p)^2}       \tab TRUE               \tab FALSE                   \tab for weighted mean squared error                                  \cr
##' \code{wRMSE} \tab \code{r * (r - p)^2}       \tab TRUE               \tab TRUE                    \tab for root weighted mean squared error                                  \cr
##' }
##'
##' @aliases luk gdl prd and wMAE wMSE wRMSE operators
##' @param p prediction vector, matrix, or array with numeric values in [0, 1]
##' @param r reference vector, matrix, or array with numeric values in [0, 1]
##' @return numeric of the same size as p
##' @author Claudia Beleites
##' @seealso Performance measures: \code{\link{sens}}
##' @references see the literature in \code{citation ("softperformance")}
##' @rdname operators
##' @export
##' @include softperformance.R
##' @include dev.R
##' @include postproc.R
##'
##' @examples
##' ops <- c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE")
##' 
##' ## make a nice table
##' t (sapply (ops, function (x){
##'   x <- c (attributes (get (x)) [c ("source", "dev", "postproc")])
##'   names (x) <- c ("source", "dev", "postproc")
##'   x
##' }))
##' 
##' 
##' x <- softperformance:::v
##' x
##' 
##' luk (0.7, 0.8)
##' 
##' ## The behaviour of the operators
##' ## op (x, 1)
##' cbind (x, sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
##'                   function (op, x) get (op) (x, 1), x))
##' 
##' ## op (x, 0)
##' cbind (x, sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
##'                   function (op, x) get (op) (x, 0), x))
##' 
##' ## op (x, x)
##' cbind (x, sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
##'                   function (op, x) get (op) (x, x), x))
##' 
##' 
##' ## Note that the deviation operators are not commutative
##' ## (due to the weighting by reference)
##' zapsmall (
##' cbind (sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
##'                   function (op, x) get (op) (1, x), x)) -
##' cbind (sapply (c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"),
##'                   function (op, x) get (op) (x, 1), x)) 
##' )
##' 
##' 
luk <- function (r, p) pmax (r + p - 1, 0)
dev (luk) <- FALSE


.test (luk) <- function(){
  checkEqualsNumeric (luk (v, v),       c (a = 0,  b = 0,   c = 0.4, d = 1,   e = NA))
  checkEqualsNumeric (luk (v, rev (v)), c (a = NA, b = 0.3, c = 0.4, d = 0.3, e = NA))
}

##' @rdname operators
##' @export 
gdl <- function (r, p)
  pmin (p, r)                           # Note: takes attributes from p only
  
dev (gdl) <- FALSE
 
.test (gdl) <- function(){
  checkEqualsNumeric (gdl (v, v),       v)
  checkEqualsNumeric (gdl (v, rev (v)), c (a = NA, b = 0.3, c = 0.7, d = 0.3, e = NA))
}

##' @rdname operators
##' @export 
prd <- function (r, p)  r * p
dev (prd) <- FALSE

.test (prd) <- function(){
  checkEqualsNumeric (prd (v, v),       v^2)
  checkEqualsNumeric (prd (v, rev (v)), c (a = NA, b = 0.3, c = 0.49, d = 0.3, e = NA))
}

.make01 <- function (x, tol = 1e-6){
  tmp <- rep (NA_real_, length (x))
  tmp [x >    -tol & x <     tol] <- 0
  tmp [x > 1 - tol & x < 1 + tol] <- 1
  
  attributes (tmp) <- attributes (x)

  tmp
}
.test (.make01) <- function (){
  checkIdentical (.make01 (v), c( a = 0, b = NA, c = NA, d = 1, e = NA))
  checkIdentical (attributes (.make01 (m)), attributes (m))
}


##' @rdname operators
##' @export 
and <- function (r, p){
  mostattributes (r) <- attributes (p)  
  p <- .make01 (p)
  r <- .make01 (r)
    
  r * p ## fastest
}
dev (and) <- FALSE

.test (and) <- function(){
  checkEqualsNumeric (and (v, v),       c (a = 0 , b = NA, c = NA, d =  1, e = NA))
  checkEqualsNumeric (and (v, rev (v)), c (a = NA_real_, b = NA, c = NA, d = NA, e = NA))
  checkEqualsNumeric (and (0, 1), 0)
}

##' @rdname operators
##' @export 
wMAE <- function (r, p) {
  mostattributes (r) <- attributes (p)
  r * abs (p - r)
}

dev (wMAE) <- TRUE

.test (wMAE) <- function(){
  checkEqualsNumeric (wMAE (v, v),       c (a = 0 , b = 0,    c = 0, d = 0,   e = NA))
  checkEqualsNumeric (wMAE (v, rev (v)), c (a = NA, b = 0.21, c = 0, d = 0.7, e = NA))
}


##' @rdname operators
##' @export 
wMSE <- function (r, p){
  r * (p - r)^2
}
dev (wMSE) <- TRUE

##' @rdname operators
##' @export 
wRMSE <- wMSE
postproc (wRMSE) <- "sqrt"


##' @nord
testoperators <- svTest (function (){
  ops <- c ("luk", "gdl", "prd", "and", "wMAE", "wMSE", "wRMSE")

  ## dev
  for (o in c ("luk", "gdl", "prd", "and"))
    checkTrue (! dev (get (o)),
               msg = sprintf ("dev: %s", o))
  for (o in c ("wMAE", "wMSE", "wRMSE"))
      checkTrue (dev (get (o)),
               msg = sprintf ("dev: %s", o))

  ## postproc
  for (o in c ("luk", "gdl", "prd", "and", "wMAE", "wMSE"))
    checkTrue (is.null (postproc (get (o))),
               msg = sprintf ("postproc: %s", o))
  
  checkEquals (postproc (wRMSE), "sqrt")

  checkEquals (sapply (ops, function (x) get (x) (1, v)),
               matrix (c (0,   0,   0,   0,   1,   1,    1,
                          0.3, 0.3, 0.3, NA,  0.7, 0.49, 0.49, 
                          0.7, 0.7, 0.7, NA,  0.3, 0.09, 0.09,
                          1,   1,   1,   1,   0,   0,    0,
                          NA,  NA,  NA,  NA,  NA,  NA,   NA), byrow = TRUE, nrow = 5,
                       dimnames = list (names (v), ops))
               )

  checkEquals (sapply (ops, function (x) get (x) (0, v)),
               matrix (c (0,   0,   0,   0,   0,   0,    0,
                          0,   0,   0,   NA,  0,   0,    0,
                          0,   0,   0,   NA,  0,   0,    0,
                          0,   0,   0,   0,   0,   0,    0,
                          NA,  NA,  NA,  NA,  NA,  NA,   NA), byrow = TRUE, nrow = 5,
                       dimnames = list (names (v), ops))
               )

  r <- runif (1000)
  p <- runif (1000)
  for (o in c ("luk", "gdl", "prd", "wMAE", "wMSE", "wRMSE"))
    checkTrue (get (o) (r, p) <= r, msg = sprintf ("op (p, r) <= r: %s", o))

  for (o in ops){
    op <- get (o)
    checkEqualAttributes (op (runif (nrow (m)), makeNd (m, 3)),
                          makeNd (m, 3),
                          msg = sprintf ("preserve shape of p: %s", o))

    checkEqualsNumeric (op (m [, 1], m),
                        op (m [, rep (1, ncol(m))], m),
                        msg = sprintf ("recycling: %s", o))
  }
  
  tmp <- runif (length (m))
  mostattributes (tmp) <- attributes (m)
  for (o in c ("luk", "gdl", "prd", "and")){
    op <- get (o)
    checkEquals (op (m, tmp),
                 op (tmp, m),
                 msg = sprintf ("commutativity: %s", o))
  }
  
})

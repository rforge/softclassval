##' Soft classification performance measures
##'
##' Extension of sensitivity, specificity, positive and negative predictive value to continuous
##' predicted and reference memberships in [0, 1]
##' @name softperformance-package
##' @docType package
##' @include init.R
##' 
{
  if (!require (svUnit)){
    `.test<-` <- function (f, value) {
      attr (f, "test") <- value
      f
    }
  } else {
    `.test<-` <- `test<-`

    checkEqualsOrdered <- function (target, current, ...)
      checkEquals (target [order (names (target))], current [order (names (current))], ...)

    checkEqualAttributes <- function (target, current, ...)
      checkEqualsOrdered (attributes (target), attributes (current), ...) # TODO: exclusion list
    
    
  }
    
## construct some test data:
## classical hard reference (5 cases)
## soft reference (5 cases)
##
##     pred     ref   correct/wrong
##  1. hard    hard   correct
##  2. hard    hard   wrong
##  3. soft    hard   wrong
##  4. soft    hard   wrong
##  5. soft    hard   wrong
##  6. hard    soft   wrong
##  7. hard    soft   wrong
##  8. soft    soft   correct
##  9. soft    soft   wrong
## 10. soft    soft   wrong


  v <- c (0, 0.3, 0.7, 1, NA)
  names (v) <- letters [1:5]
  
  m <- matrix (c (1  , 0  ,  0,
                  0.7, 0.3,  0,
                  NA,  1  ,  1,
                  0.3, 0.7, NA),
               ncol = 3, byrow = TRUE)
  colnames (m) <- LETTERS [1 : 3]
  rownames (m) <- letters [1 : 4]  
  
ref <- matrix (c (rep (c( 1  , 0  , 0), 5),
                   rep (c (0.6, 0.4, 0), 5)),
                ncol = 3, byrow = TRUE)
colnames (ref) <- LETTERS [1 : 3]

pred <- matrix (rep (c (1  , 0  , 0,
                         0  , 1  , 0,
                         0.6, 0.4, 0,
                         0.6, 0.1, 0.3,
                         0.8, 0.1, 0.1), 2),
               ncol = 3, byrow = TRUE)
colnames (pred) <- letters [1 : 3]

ref.array <- array (rep (ref, 2), dim = c (dim (ref), 2),
                     dimnames = c (dimnames (ref), list (1 : 2)))
  
pred.array <- array (c (pred, ref), dim = c (dim (ref), 2),
                     dimnames = c (dimnames (pred), list (1 : 2)))
}



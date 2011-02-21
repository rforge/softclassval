\name{stack}
\alias{peek}
\title{A little stack.}
\usage{peek(x, an, n=1)
pop(x, an, n=1)
push (x, an) <- value}
\description{A little stack.}
\details{\code{peek}: TODO: implement as reference class?
Note: \code{pop} only removes elements. To retrieve them, use \code{peek}.

}
\value{\code{peek}: \code{push} and \code{pop}: the object with stack in list \code{an} pushed/popped by the \code{n} elements

\code{peek}: the \code{n}th stack element (without popping!)

}
\author{Claudia Beleites}
\arguments{\item{x}{the object}
\item{an}{attribute holding the stack}
\item{n}{numer of element to peek at and numer of elements to pop (delete), respectively}
\item{value}{list of things to push on the stack.}
}
\alias{pop}
\alias{push<-}


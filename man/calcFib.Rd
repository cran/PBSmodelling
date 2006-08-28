\name{calcFib}
\alias{calcFib}
\title{Several Methods to Calculate Fibonacci Numbers}

\description{
  Computes Fibonacci numbers using four different methods:
  1) iteratively using R code,
  2) via the closed function in R code,
  3) iteratively in C using the \code{.C} function,
  and 4) iteratively in C using the \code{.Call} function.
}

\usage{
calcFib(n, len=1, method="C")
}

\arguments{
  \item{n}{nth fibonacci number to calculate}
  \item{len}{A vector of length \code{len} showing previous fibonacci numbers}
  \item{method}{Select method to use: \code{C}, \code{Call}, \code{R}, \code{closed}}
}

\value{Vector of the last \code{len} Fibonacci numbers 
  calculated.
}

\keyword{arith}

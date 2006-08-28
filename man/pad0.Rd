\name{pad0}
\alias{pad0}
\title{Pads Numbers with leading zeroes}

\description{
  Takes numbers, converts them to integers then text, and pads them with leading zeroes. 
}

\usage{
pad0(x, n, f = 0) 
}

\arguments{
  \item{x}{Vector of numbers}
  \item{n}{Length of padded integer}
  \item{f}{Factor of 10 to expand x by}
}

\value{
  A character vector representing x with leading zeros.
}

\keyword{print}

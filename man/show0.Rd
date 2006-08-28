\name{show0}
\alias{show0}
\title{Convert Numbers Into Text With Specified Decimal Places}

\description{
  Return character representation of number with specified decimal places.
}

\usage{
show0(x, n, add2int = FALSE) 
}

\arguments{
  \item{x}{Number as scalar or vector}
  \item{n}{Number of decimal places to show, include zeroes}
  \item{add2int}{If TRUE, add zeroes on the end of integers}
}

\examples{
frame()

#do not show decimals on integers
addLabel(0.25,0.75,show0(15.2,4))
addLabel(0.25,0.7,show0(15.1,4))
addLabel(0.25,0.65,show0(15,4))

#show decimals on integers
addLabel(0.25,0.55,show0(15.2,4,TRUE))
addLabel(0.25,0.5,show0(15.1,4,TRUE))
addLabel(0.25,0.45,show0(15,4,TRUE))
}

\keyword{print}

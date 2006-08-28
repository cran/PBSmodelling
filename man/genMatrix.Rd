\name{genMatrix}
\alias{genMatrix}
\title{Generate Test Matrices for plotBubbles}

\description{
  Generates a test matrix of random numbers (mean = mu
  and standard deviation = sigma), primarily for plotBubbles.
}

\usage{
genMatrix(m,n,mu=0,sigma=1) 
}

\arguments{
  \item{m}{number of rows}
  \item{n}{number of columns}
  \item{mu}{mean of normal distribution}
  \item{sigma}{standard deviation of normal distribution}
}

\value{
  An m by n matrix with normally distributed random values.
}

\seealso{
  \code{\link{plotBubbles}}
}

\examples{
plotBubbles(genMatrix(20,6))
}

\keyword{array}

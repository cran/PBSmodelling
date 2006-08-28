\name{plotBubbles}
\alias{plotBubbles}
\title{Construct a Bubble Plot from a Matrix}

\description{
  Constructs a bubble plot for a matrix z.
}

\usage{
plotBubbles(z, xval = FALSE, yval = FALSE, rpro = FALSE, 
cpro = FALSE, rres = FALSE, cres = FALSE, powr = 1, 
clrs = c("black", "red"), size = 0.2, lwd = 2, debug = FALSE, ...)
}

\arguments{
  \item{z}{input matrix}
  \item{xval}{x-values for the columns of z. if xval=TRUE, first row contains x-values for the columns}
  \item{yval}{y-values for the rows of z. if yval=TRUE, first column contains y-values for the rows}
  \item{rpro}{if rpro=TRUE, convert rows to proportions}
  \item{cpro}{if cpro=TRUE, convert columns to proportions}
  \item{rres}{if rres=TRUE, use row residuals (subtract row means)}
  \item{cres}{if cres=TRUE, use column residuals (subtract column means)}
  \item{powr}{power transform. radii proportional to \eqn{z^{powr}}. powr=0.5 gives bubble areas proportional to z}
  \item{clrs}{colours used for positive and negative values}
  \item{size}{size (inches) of the largest bubble}
  \item{lwd}{line width for drawing circles}
  \item{debug}{display debug information if true}
  \item{...}{additional arguments for symbols function}
}

\seealso{
  \code{\link{genMatrix}}
}

\examples{
plotBubbles(genMatrix(20,6))
}

\keyword{hplot}

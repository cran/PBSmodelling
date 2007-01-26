\name{plotBubbles}
\alias{plotBubbles}
\title{Construct a Bubble Plot from a Matrix}

\description{
	Construct a bubble plot for a matrix \code{z}.
}

\usage{
plotBubbles(z, xval = FALSE, yval = FALSE, rpro = FALSE, 
   cpro = FALSE, rres = FALSE, cres = FALSE, powr = 1, 
   clrs = c("black", "red"), size = 0.2, lwd = 2, debug = FALSE, ...)
}

\arguments{
	\item{z}{input matrix}
	\item{xval}{x-values for the columns of \code{z}. 
		if \code{xval=TRUE}, the first row contains x-values for the columns.}
	\item{yval}{y-values for the rows of \code{z}. 
		If \code{yval=TRUE}, the first column contains y-values for the rows.}
	\item{rpro}{logical; if \code{TRUE}, convert rows to proportions.}
	\item{cpro}{logical; if \code{TRUE}, convert columns to proportions.}
	\item{rres}{logical; if \code{TRUE}, use row residuals (subtract row means).}
	\item{cres}{logical; if \code{TRUE}, use column residuals (subtract column means).}
	\item{powr}{power transform. Radii are proportional to \code{z^powr}. 
		Note: \code{powr=0.5} yields bubble areas proportional to \code{z}.}
	\item{clrs}{colours (2-element vector) used for positive and negative values.}
	\item{size}{size (inches) of the largest bubble.}
	\item{lwd}{line width for drawing circles.}
	\item{debug}{logical; if \code{TRUE}, display debug information.}
	\item{...}{additional arguments for \code{symbols} function.}
}

\seealso{
	\code{\link{genMatrix}}
}

\examples{
plotBubbles(genMatrix(20,6),clrs=c("green","red"));
}

\keyword{hplot}

\name{testPch}
\alias{testPch}
\title{Display Plotting Symbols}

\description{
  Allows the user to specify particular ranges (increasing continuous integer) for pch.
}

\usage{
testPch(pch=1:100, ncol=10, grid=TRUE, newframe=TRUE, bs=FALSE)
}

\arguments{
  \item{pch}{symboles to view}
  \item{ncol}{number of columns (can only be 2, 5, or 10). Most sensibly this is set to 10.}
  \item{grid}{if T, grid is plotted for visual aid}
  \item{newframe}{if T,reset the graph, otherwise overlay on top of the current graph}
  \item{bs}{if T, shows backslash characters used in text statements. (e.g. \code{30\\272C} = 30\eqn{^\circ}{°}C)}
}

\examples{
testPch(123:255)
testPch(1:25,ncol=5)
testPch(41:277,bs=TRUE)
}

\keyword{utilities}
\keyword{color}

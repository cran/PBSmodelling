\name{drawBars}
\alias{drawBars}
\title{Draw a Linear Barplot on the Current Plot}

\description{
  Draws a linear barplot on the current graph.
}
\usage{drawBars(x, y, width, base = 0, ...)}

\arguments{
  \item{x}{x-coordinates}
  \item{y}{y-coordinates}
  \item{width}{bar width, computed if missing}
  \item{base}{y-value of the base of each bar}
  \item{...}{further graphical parameters (see 'par') may also be supplied as arguments}
}
\examples{
plot(0:10,0:10,type="n")
drawBars(x=1:9,y=9:1,col="deepskyblue4",lwd=3)
}

\keyword{hplot}

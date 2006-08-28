\name{testCol}
\alias{testCol}
\title{Display Colour Palette}

\description{
  Provides a testing bed for displaying colours on a graph.
  Colours can be specified in any of 3 different ways:
  1) by a color name,
  2) by a hexidecimal color code created by \code{rgb()}
  or 3) by an index into the color palette.
}

\usage{
testCol(cnam=colors()[1:20]) 
}

\arguments{
  \item{cnam}{vector of colour names to display}
}

\seealso{
  \code{\link{pickCol}}
}

\examples{
testCol(c("sky","fire","sea","wood"))

testCol(c("plum","tomato","olive","peach","honeydew"))

testCol(rainbow(63))

#display all colours set in the colour palette
testCol(1:length(palette()))

#they can even be mixed
testCol(c("#9e7ad3", "purple", 6))
}

\keyword{utilities}
\keyword{color}

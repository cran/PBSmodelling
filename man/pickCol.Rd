\name{pickCol}
\alias{pickCol}
\title{Display Interactive Colour Selection Palette}

\description{
  Display an interactive colour chooser. If returnValue is true, then the
  equivalent hex colour value is returned, otherwise if returnValue is faluse, 
  an intermediate GUI is used to display the hex value and no value is returned to R.
}

\usage{pickCol(returnValue=TRUE)}

\arguments{
  \item{returnValue}{If true only display the colour chooser and return the hex value.
    Otherwise use an intermediate GUI to display the hex value}
}

\value{
  A hexidecimal colour value.
}

\seealso{
  \code{\link{testCol}}
}

\keyword{color}

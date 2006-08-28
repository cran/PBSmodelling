\name{testLwd}
\alias{testLwd}
\title{Display Line Widths}

\description{
  User can specify particular ranges for lwd. Colours can also be
  specified and are internally repeated as necessary.
}

\usage{
testLwd(lwd=1:20, col=c("black","blue"), newframe=TRUE) 
}

\arguments{
  \item{lwd}{line widths to test}
  \item{col}{colours to test}
  \item{newframe}{if true, create a new blank frame, otherwise overlay current frame}
}

\examples{
testLwd(3:15,col=c("salmon","aquamarine","gold"))
}

\keyword{utilities}
\keyword{color}

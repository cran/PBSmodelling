\name{parseWinFile}
\alias{parseWinFile}
\title{Convert Window Description File into a List}

\description{
  Parses a Window Markup file into the list format expected by \code{createWin()}
}

\note{
  All widgets are encapsulated into a 1 column by N row grid.
}

\usage{
parseWinFile(fname, astext=FALSE)
}

\arguments{
  \item{fname}{file name of window markup file.}
  \item{astext}{if true, fname is interpreted as a vector of strings. with each element representing a line of the source of a window description file}
}

\value{
  A list representing a parsed window description file 
  that can be directly passed to createWin.
}

\seealso{
  \code{\link{createWin}}
  
  \code{\link{compileDescription}}
}

\examples{
\dontrun{
x<-parseWinFile(system.file("examples/LissFigWin.txt",package="PBSmodelling"))
createWin(x)
}
}

\keyword{misc}

\name{readList}
\alias{readList}
\title{Read a List From a File}

\description{
  Reads in a list which was saved to a file by writeList. It can support a native R list, 
  or PBSmodelling P format. It is detected automatically.

  For information about \code{"P"} format, see \link{writeList}.
}

\usage{readList(fname)}

\arguments{
  \item{fname}{file name of text file containing the list.}
}

\seealso{
  \code{\link{writeList}}
  
  \code{\link{unpackList}}
}

\keyword{list}
\keyword{file}

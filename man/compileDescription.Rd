\name{compileDescription}
\alias{compileDescription}
\title{Converts and Saves a Window Description Into a List}

\description{
  compileDescription converts a Window Description File into an 
  equivalent Window Description List.  The list is complete - 
  meaning all default values have been added to the list.
}

\usage{compileDescription(descFile, outFile)}

\arguments{
  \item{descFile}{filename of markup file.}
  \item{outFile}{filename indicating where to save outputed R source code.}
}

\details{
  The Window Description File is converted into a list, which is then converted into R code
  and saved to the outfile.
}

\seealso{
  \code{\link{parseWinFile}}

  \code{\link{createWin}}
}

\keyword{utilities}

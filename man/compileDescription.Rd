\name{compileDescription}
\alias{compileDescription}
\title{Convert and Save a Window Description as a List}

\description{
	Convert a Window Description File (ASCII markup file) to an equivalent 
	Window Description List. The output list (an ASCII file containing R-source code) 
	is complete, i.e., all default values have been added.
}

\usage{compileDescription(descFile, outFile)}

\arguments{
	\item{descFile}{file name of markup file.}
	\item{outFile}{file name of output file containing R source code.}
}

\details{
	The Window Description File \code{descFile} is converted to a list, 
	which is then converted to R code, and saved to \code{outFile}.
}

\seealso{
  \code{\link{parseWinFile}}, \code{\link{createWin}}
}

\keyword{utilities}

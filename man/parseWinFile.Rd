\name{parseWinFile}
\alias{parseWinFile}
\title{Convert a Window Description File into a List Object}

\description{
	Parse a Window Description (markup) File into the list format expected by \code{createWin()}.
}

\note{
	All widgets are forced into a 1-column by N-row grid.
}

\usage{
parseWinFile(fname, astext=FALSE)
}

\arguments{
	\item{fname}{file name of the Window Description File.}
	\item{astext}{if \code{TRUE}, \code{fname} is interpreted as a vector of strings, 
		with each element representing a line of code in a Window Description File.}
}

\value{
	A list representing a parsed Window Description File that can be directly 
	passed to \code{createWin}.
}

\seealso{
	\code{\link{createWin}}, \code{\link{compileDescription}}
}

\examples{
\dontrun{
x<-parseWinFile(system.file("examples/LissFigWin.txt",package="PBSmodelling"))
createWin(x)
}
}

\keyword{misc}

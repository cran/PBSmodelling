\name{getPBSext}
\alias{getPBSext}
\title{Get a Command Associated With a Filename}

\description{
	Display all locally defined file extensions and their associated commands, 
	or search for the command associated with a specific file extension \code{ext}.
}

\note{
	These file associations are not saved from one \emph{PBS Modelling} session to the next.
}

\usage{getPBSext(ext)}

\arguments{
	\item{ext}{optional string specifying a file extension (suffix)}
}

\value{
	Command associated with file extension.
}

\seealso{
	\code{\link{setPBSext}}, \code{\link{openFile}}
}

\keyword{methods}

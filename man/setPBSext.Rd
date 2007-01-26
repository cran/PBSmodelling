\name{setPBSext}
\alias{setPBSext}
\title{Set a Command Associated with a Filename Extension}

\description{
	Set a command with an associated extension (suffix), for use in 
	\code{openFile}. The command must specify where the target file 
	name is inserted by indicating a \code{\%f}.
}

\note{
	These values are not saved from one PBS Modelling session to the next.
}

\usage{
setPBSext(ext, cmd)
}

\arguments{
	\item{ext}{string of specifying the extension suffix.}
	\item{cmd}{command string to associate with the extension.}
}

\seealso{
	\code{\link{getPBSext}}, \code{\link{openFile}}
}

\keyword{methods}

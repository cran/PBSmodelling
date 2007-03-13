\name{runDemos}
\alias{runDemos}
\title{Interactive GUI for R demos}

\description{
	An interactive GUI for accessing demos from any R package installed on the 
	user's system. \code{runDemos} is a convenient alternative to R\'s \code{demo} 
	function.
}
\usage{runDemos(package)}

\arguments{
	\item{package}{ display demos from a particular package (optional)}
}
\details{
	If the argument \code{package} is not specified, the function will look for 
	demos in all packages installed on the user's system.
}
\note{
	The \code{runDemos} GUI attempts to retain the user's objects and restore
	the working directory. However, pre-existing objects will be overwritten 
	if their names co-incide with names used by the various demos. Also, 
	depending on conditions, the user may lose working directory focus. 
	We suggest that users run this demo from a project where data objects are 
	not critical. \bold{--- USER BEWARE ---}
}
\seealso{
	\code{\link{runExamples}}  for examples specific to PBSmodelling.
}
\keyword{utilities}

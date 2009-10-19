\name{findProgram}
\alias{findProgram}
\title{Locates a program in the PATH environment variable }
\description{
  Returns the complete filename and path of a program in the PATH environment variable.
  findProgram is similar to the which command under Unix, but works under Windows too.
}
\usage{
findProgram( name, includename=FALSE )
}
\arguments{
	\item{name}{name of a program to locate}
	\item{includename}{boolean: if true, include the filename in the path returned, otherwise just the directory.}
}
\value{
  A string containing the location of the program. NULL is returned if the program is not located.
}
\examples{
findProgram( "gcc" )
findProgram( "notepad" )
findProgram( "R", TRUE )
}
\author{
  Alex Couture-Beil
}
\seealso{
  \code{\link{dirname}}
}
\keyword{file}
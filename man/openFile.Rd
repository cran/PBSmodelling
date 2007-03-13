\name{openFile}
\alias{openFile}
\title{Open a File with the Associated Program}

\description{
	Open a file using the program associated with its extension defined by 
	the Windows shell. Non-windows users, or users wishing to overide the 
	default application, can specify a program association using \code{\link{setPBSext}}.
}

\usage{openFile(fname)}

\note{
	If a command is registered with \code{setPBSext}, then \code{openFile} 
	will replace all occurrences of \code{"\%f"} with the absolute path of 
	the filename, before executing the command.
}

\arguments{
	\item{fname}{file name of file to open.}
}

\seealso{
  \code{\link{getPBSext}}, \code{\link{setPBSext}}
}

\examples{
\dontrun{
# Set up firefox to open .html files
setPBSext("html", '"c:/Program Files/Mozilla Firefox/firefox.exe" file://\%f')
openFile("foo.html")
}
}

\keyword{file}

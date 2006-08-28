\name{openFile}
\alias{openFile}
\title{Open Files With Associated Program}

\description{
  openFile attempts to open a file, based off the command set in the \code{PBS.options\$openfile} list.
  If \code{PBS.options\$openfile[[extension]]} is set openFile will replace all occurrences of \code{"\%f"} with
  the absolute path of the filename, and then execute the command. Otherwise, if no command is set, 
  \code{shell.exec()} will be used.
}

\usage{openFile(fname)}

\arguments{
  \item{fname}{filename to be opened.}
}

\examples{
\dontrun{
#setup firefox to open .html files
PBS.options$openfile$html='"c:/Program Files/Mozilla Firefox/firefox.exe" \%f'
openFile("foo.html")
}
}

\keyword{file}

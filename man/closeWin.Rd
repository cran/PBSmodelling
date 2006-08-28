\name{closeWin}
\alias{closeWin}
\title{Close GUI windows}

\description{
  The closeWin function closes (destroys) one or more windows made with createWin.
}

\usage{closeWin(name=names(.PBS.tclHash))}

\arguments{
  \item{name}{a vector of window names to close, as defined in the window description file's WINDOW widget.}
}

\seealso{
  \code{\link{createWin}}
}

\keyword{utilities}

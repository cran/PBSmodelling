\name{closeWin}
\alias{closeWin}
\title{Close GUI Window(s)}

\description{
	Close (destroy) one or more windows made with \code{createWin}.
}

\usage{closeWin(name)}

\arguments{
	\item{name}{a vector of window names that indicate which windows to close. These 
		names appear in the Window Description File(s) on the line(s) defining WINDOW widgets.
		If \code{name} is ommitted, all active windows will be closed.}
}

\seealso{
	\code{\link{createWin}}
}

\keyword{utilities}

\name{exportPBShistory}
\alias{exportPBShistory}
\title{Export a Saved History}

\description{
  Export the current history list.
}

\usage{
exportPBShistory(hisname="", fname="")
}

\arguments{
	\item{hisname}{name of the history list to export. If set to \code{""}, 
		the value from \code{getWinAct()[1]} will be used instead.}
	\item{fname}{file name where history will be saved. If it is set to \code{""}, 
		a \bold{Save As} window will be displayed.}
}

\seealso{
	\code{\link{importPBShistory}}, \code{\link{initPBShistory}}, \code{\link{promptSaveFile}}
}

\keyword{misc}

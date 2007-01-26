\name{importPBShistory}
\alias{importPBShistory}
\title{Import a History List from a File}

\description{
	Import a history list from file \code{fname}, and place it 
	into the history list \code{hisname}.
}

\usage{
importPBShistory(hisname="", fname="")
}

\arguments{
	\item{hisname}{name of the history list to be populated. 
		The default (\code{""}) uses the value from \code{getWinAct()[1]}.}
	\item{fname}{file name of history file to import. 
		The default (\code{""}) causes an open-file window to be displayed.}
}

\seealso{
\code{\link{exportPBShistory}}, \code{\link{initPBShistory}}, \code{\link{promptOpenFile}}
}

\keyword{misc}

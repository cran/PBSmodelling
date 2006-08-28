\name{exportPBShistory}
\alias{exportPBShistory}
\title{Export Saved History}

\description{
  Exports the current history List.
}

\usage{
exportPBShistory(hisname="", fname="")
}

\arguments{
  \item{hisname}{Name of the history list to export. If it is set to \code{""}, the value from \code{PBS.win\$action[1]} will be used instead.}
  \item{fname}{Where to save the history to. If it is set to \code{""}, a save file window will be displayed.}
}

\seealso{
  \code{\link{importPBShistory}}
  
  \code{\link{initPBShistory}}
  
  \code{\link{promptSaveFile}}
}

\keyword{misc}

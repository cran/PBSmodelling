\name{importPBShistory}
\alias{importPBShistory}
\title{Import a history list from a file}

\description{
  Imports a history list from a file, and places it as the history list identified by hisname.
}

\usage{
importPBShistory(hisname="", fname="")
}

\arguments{
  \item{hisname}{The imported list is placed into the history list identified by hisname. If it is set to \code{""}, the value from \code{PBS.win\$action[1]} will be used instead.}
  \item{fname}{Which file to import. If it is set to \code{""}, an open file window will be displayed.}
}

\seealso{
  \code{\link{exportPBShistory}}

  \code{\link{initPBShistory}}

  \code{\link{promptOpenFile}}
}

\keyword{misc}

\name{getWinVal}
\alias{getWinVal}
\title{Retreive Widget Values}

\description{
  An optional scope argument allows the function to create local or global variables
  based on the list that is returned.
}

\usage{
getWinVal(v=NULL, scope="", asvector=FALSE, windowname="")
}

\arguments{
  \item{v}{vector of variable names to be retrieved. If NULL, it is set to every widget.}
  \item{scope}{If \code{""}, do not set any variables. 
    If \code{"L"} create variables local to the parent frame that called the function.
    If \code{"G"} create global variables.}
  \item{asvector}{return a vector instead of a list. WARNING: if a widget variable is a true vector or matrix, this will not work.}
  \item{windowname}{Which window to select values from. If \code{""} is given, it will use the most recently active window determined from \code{PBS.win\$windowname}.}
}

\value{
  A list (or vector) with named components, based on the variable
  name as the key, and the value of the associated widget as the value.
}

\seealso{
  \code{\link{parseWinFile}}
  
  \code{\link{setWinVal}}
  
  \code{\link{clearWinVal}}
}

\keyword{methods}

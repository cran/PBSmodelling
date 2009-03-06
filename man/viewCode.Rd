\name{viewCode}
\alias{viewCode}
\title{ View Package R Code }
\description{
  View the R code of all functions in a specified package 
  installed on the user's system.
}
\usage{
viewCode(pkg="PBSmodelling")
}
\arguments{
  \item{pkg}{ string name of an installed package }
}
\value{
  Invisibly returns source code of all functions in the specified package.
  The function invokes \code{openFile} to display the results.
}
\author{ Rowan Haigh, Pacific Biological Station, Nanaimo BC }
\keyword{ character }
\keyword{ package }

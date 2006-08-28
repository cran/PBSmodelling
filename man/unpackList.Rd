\name{unpackList}
\alias{unpackList}
\title{Unpack List Elements Into Variables}

\description{
  Function to make local or global variables (depending on the scope) from the named components of a list.
}

\usage{unpackList(x, scope="L")}

\arguments{
  \item{x}{list to unpack.}
  \item{scope}{If "L" create variables local to the parent frame that called the function.
    If "G" create global variables.}
}

\value{
  A character vector of unpacked variable names.
}

\seealso{
  \code{\link{readList}}
}

\examples{
x<-list(a=21,b=23)
unpackList(x)
print(a)
}

\keyword{file}
\keyword{list}

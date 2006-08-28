\name{setWinVal}
\alias{setWinVal}
\title{Update Widget Values}

\description{
  setWinVal updates a widget with a new value. The vars argument expects a list or vector with named elements.
  Every element name corresponds to the widget name which will be updated with the supplied element value.

  The vector and matrix widgets can be updated in several ways. If more than one name is given for the names argument
  then each element is treated as if it simply an entry widget. If however, a single name is given, and the value returned
  by getWinVal is a vector or matrix, the whole widget can be updated by passing an appropriately sized vector or widget.
  Alternatively each element can be updated by appending the index in square braces to the end of the name.
}

\usage{
setWinVal(vars, windowname)
}

\arguments{
  \item{vars}{a list or vector with named components.}
  \item{windowname}{Which window to select values from. If \code{""} is given, it will use the most recently active window determined from \code{PBS.win\$windowname}.}
}

\details{
  The data widget can also be updated in the same fashion as the matrix; however, when updating a single element, a "d" must be 
  added after the brackets. This is due to the internal coding of PBS Modeling. Example: \code{"foo[1,1]d"}
}

\seealso{
  \code{\link{getWinVal}}
  
  \code{\link{createWin}}
}

\examples{
winDesc <- c(
            "vector length=3 name=vec",
            "matrix nrow=2 ncol=2 name=mat",
            "slideplus name=foo"
            );
createWin(winDesc, astext=TRUE)
setWinVal(list(vec=1:3, "mat[1,1]"=123, foo.max=1.5, foo.min=0.25, foo=0.7))
}

\keyword{methods}

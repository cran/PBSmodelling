\name{createVector}
\alias{createVector}
\title{Create a GUI with a Vector Widget}

\description{
  createVector creates a basic window containing a vector and a submit button.
  This provides a quick way to create a window without the need for a window description file.
}

\usage{createVector(vec, vectorLabels=NULL, func="", windowname="vectorwindow")}

\arguments{
  \item{vec}{a vector of strings representing widget variables. 
    If it is named, the names are used as the variable names, 
    and the values are used as the default value of the widget}
  \item{vectorLabels}{option vector of strings to be used as labels above each widget.}
  \item{func}{string value of function name to be called when new data is entered or when "GO" is pressed.},
  \item{windowname}{window name required if multiple vector windows are created.}
}

\seealso{
  \code{\link{createWin}}
}

\examples{
#user defined function which is called on new data	
drawLiss <- function()
{
  getWinVal(scope="L")
	
  tt <- 2*pi*(0:k)/k;
	
  x <- sin(2*pi*m*tt); 
  y <- sin(2*pi*(n*tt+phi));
	
  plot(x,y,type="p");
	
  invisible(NULL); 
}

#create the vector window
createVector(c(m=2, n=3, phi=0, k=1000), 
             vectorLabels=c("x cycles","y cycles", "y phase", "points"), 
             func="drawLiss"
             )
}

\keyword{utilities}

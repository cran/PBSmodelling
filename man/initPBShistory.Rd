\name{initPBShistory}
\alias{initPBShistory}
\alias{rmPBShistory}
\alias{addPBShistory}
\alias{forwPBShistory}
\alias{backPBShistory}
\alias{jumpPBShistory}
\alias{clearPBShistory}
\title{Customized History Widget Functions}

\description{
  The functions: initPBShistory, rmPBShistory, addPBShistory, forwPBShistory, backPBShistory, jumpPBShistory are made available to
  those who would like to use history, without using the history widget. In other words, these functions allow users to create a custom history widget.
}

\usage{
initPBShistory(hisname, indexname=NULL, sizename=NULL, overwrite=TRUE)
rmPBShistory(hisname="", index="")
addPBShistory(hisname="")
forwPBShistory(hisname="")
backPBShistory(hisname="")
jumpPBShistory(hisname="", index="")
clearPBShistory(hisname="")
}

\arguments{
  \item{hisname}{The name of the history "list" to manipulate. If it is omitted, the function
    will use the value of \code{PBS.win\$actions[1]} as the history name.
    This allows functions to be called directly from the window description file. With the exception of \code{initPBShistory}
    which must be called before \code{createWin()}.}
  \item{indexname}{The name of the index entry widget in the window description file. If it is \code{NULL}, then the current index feature will be disabled.}
  \item{sizename}{The name of the current size entry widget. If it is \code{NULL}, then the current size feature will be disabled.}
  \item{index}{Index to the history item. if it is left as \code{""}, then value will be extracted from the widget identified by \code{indexname}}
  \item{overwrite}{If set to true, history (matching \code{hisname}) will be cleared. Otherwise, the two will be merged.}
}

\details{
  PBS Modelling includes a pre-built history widget designed to collect interesting choices of
  GUI variables so that they can be redisplayed later, rather like a slide show. 

  Normally, a user would invoke a history widget simply by including a reference to it
  in the description file. However, PBS Modelling includes some support functions for customized
  applications.

  To create a customized history, each button must be described separately in the window description file rather
  than making reference to the history widget.

  The history "List" must be initialized before any other functions may be called.
  The use of a unique history name (\code{hisname}) is used to associate a unique history session with the supporting functions.
	
  The \code{indexname} and \code{sizename} arguments correspond to the given names of entry widgets in the 
  window description file which will be used to display the current index, and total size of the list. 
  the \code{indexname} entry widget can also be used by \code{jumpPBShistory} to retrieve an index to jump to.
}

\seealso{
  \code{\link{importPBShistory}}

  \code{\link{exportPBShistory}}
}

\examples{
#Example of creating a custom history widget that saves values
#whenever the "plot" button is pressed, and updates the plot when
#back or next is pushed. A custom history is needed to achieve this 
#functionality since the normal history widget does not update plots
require(PBSmodelling)

#create a window Description to be used with createWin using astext=TRUE
#PS: watch out for escaping special characters which 
#    must be done twice (first for R, then PBSmodelling)

winDesc <- '
window title="Custom History"
vector names="a b k" labels="a b points" font="bold" values="1 1 1000" function=myPlot
grid 1 3
  button function=myHistoryBack text="<- Back"
  button function=myPlot text="Plot"
  button function=myHistoryForw text="Next ->"

grid 2 2
  label "Index"
  entry name="myHistoryIndex" width=5
  label "Size"
  entry name="myHistorySize" width=5
'
#convert text into vector with each line represented as a new element
winDesc <- strsplit(winDesc, "\n")[[1]]

#custome functions required to update plots after restoring history values
myHistoryBack <- function() {
  backPBShistory("myHistory")
  myPlot(saveVal=FALSE) #show the plot with saved values
}
myHistoryForw <- function() {
  forwPBShistory("myHistory")
  myPlot(saveVal=FALSE) #show the plot with saved values
}

myPlot <- function(saveVal=TRUE) {
	
  #save all data whenever plot is called (directly)
  if (saveVal)
    addPBShistory("myHistory")
	
  getWinVal(scope="L")
  tt <- 2*pi*(0:k)/k;
  x <- (1+sin(a*tt));  y <- cos(tt)*(1+sin(b*tt));
  plot(x, y)
}


initPBShistory("myHistory", "myHistoryIndex", "myHistorySize")
createWin(winDesc, astext=TRUE)
}

\keyword{utilities}

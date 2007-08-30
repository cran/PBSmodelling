\name{getChoice}
\alias{getChoice}
\title{Choose One String Item from a List of Choices}
\description{
  Prompts the user to choose one string item from a list of 
  choices displayed in a GUI. The simplest case \code{getChoice()} 
  yields \code{TRUE} or \code{FALSE}.

}
\usage{
getChoice(question="Make a choice: ", choice=c("Yes","No"),
          winname="getChoice", horizontal=TRUE, radio=FALSE,
          qcolor="blue", gui=TRUE, quiet=FALSE)
}
\arguments{
  \item{question}{ question or prompting statement }
  \item{choice}{ vector of strings from which to choose }
  \item{winname}{ window name for the \code{getChoice} GUI }
  \item{horizontal}{ if \code{TRUE}, display the choices horizontally, else vertically }
  \item{radio}{ if \code{TRUE}, display the choices as radio buttons, else as buttons }
  \item{qcolor}{ colour for \code{question} }
  \item{gui}{ if \code{TRUE}, \code{getChoice} is functional when called from a GUI,
    else it is functional from command line programs }
  \item{quiet}{ if \code{TRUE}, don't print the choice on the command line }
}
\details{
  The user's choice is stored in \code{.PBSmod$options$getChoice} 
  (or whatever \code{winname} is supplied).
  
  \code{getChoice} generates an \code{onClose} function that returns focus
  to the calling window (if applicable) and prints out the choice.
}
\value{
  If called from a GUI (\code{gui=TRUE}), no value is returned directly. Rather, 
  the choice is written to the PBS options workspace, accessible through 
  \code{getPBSoptions("getChoice")} (or whatever \code{winname} was supplied).
  
  If called from a command line program (\code{gui=FASLE}), the choice is returned 
  directly as a string scalar (e.g., \code{answer <- getChoice(gui=F)} ). 
  
}
\note{ 
	Microsoft Windows users may experience difficulties switching focus between the 
	R console and GUI windows. The latter frequently disappear from the screen and 
	need to be reselected (either clicking on the task bar or pressing <Alt><Tab>. 
	This issue can be resolved by switching from MDI to SDI mode. From the R console 
	menu bar, select <Edit> and <GUI preferences>, then change the value of 
	\dQuote{single or multiple windows} to SDI.
}
\seealso{ 
	\code{\link{chooseWinVal}}, \code{\link{getWinVal}}, \code{\link{setWinVal}}
}
\examples{
\dontrun{
#-- Example 1
getChoice("What do you want?", c("Fame","Fortune","Health",
   "Beauty","Lunch"),qcolor="red",gui=F)

#-- Example 2
getChoice("Who`s your daddy?",c("Homer Simpson","Pierre Trudeau",
   "Erik the Red"),horiz=F,radio=T,gui=F)
}
}
\keyword{ device }
\keyword{ utilities }

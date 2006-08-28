\name{promptSaveFile}
\alias{promptSaveFile}
\title{Display Save File Dialog}

\description{
  Displays the default save file prompt provided by the Operating System.
}

\usage{
promptSaveFile(initialfile="", filetype=list(c("*", "All Files")), save=TRUE)
}

\arguments{
  \item{initialfile}{file name of text file containing the list.}
  \item{filetype}{a list of character vectors indicating what filetypes to look for. Each vector is of length one or two, and
    specifies the file extension, or \code{"*"} (for all filetypes). The second element is an optional name for the file type describing
    the file type.}
  \item{save}{If True display Save file prompt, if False display Open File prompt.}
}

\value{
  The filename and path of file selected by user.
}

\seealso{
  \code{\link{promptOpenFile}}
}

\examples{
\dontrun{

#illustrates how to set filetype.
promptSaveFile("intial_file.txt", filetype=list(c(".txt", "text files"), 
               c(".r", "R files"), c("*", "All Files")))
}
}

\keyword{misc}

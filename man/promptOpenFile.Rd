\name{promptOpenFile}
\alias{promptOpenFile}
\title{Display Open File Dialog}

\description{
  Displays the default open file prompt provided by the Operating System.
}

\usage{
promptOpenFile(initialfile="", filetype=list(c("*", "All Files")), open=TRUE)
}

\arguments{
  \item{initialfile}{file name of text file containing the list.}
  \item{filetype}{a list of character vectors indicating what filetypes to look for. Each vector is of length one or two, and
    specifies the file extension, or \code{"*"} (for all filetypes). The second element is an optional name for the file type describing
    the file type.}
  \item{open}{If True display Open file prompt, if False display Save File prompt.}
}

\value{
  The filename and path of file selected by user.
}

\seealso{
  \code{\link{promptSaveFile}}
}

\examples{
\dontrun{

#open a filename, and return it line by line in a vector
scan(promptOpenFile(),what=character(),sep="\n")


#illustrates how to set filetype.
promptOpenFile("intial_file.txt", filetype=list(c(".txt", "text files"), 
               c(".r", "R files"), c("*", "All Files")))
}
}

\keyword{misc}

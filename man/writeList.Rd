\name{writeList}
\alias{writeList}
\title{Write a List to a File}

\description{
  Writes an ASCII text representation in either \dQuote{D} format, or \dQuote{P} format. The \dQuote{D} format
  makes use of dput and dget and produces an R representation of the list. The \dQuote{P} format
  attempts to represents a simple list in an easy to read format.
}

\usage{
writeList(x, fname, format="D", comments="")
}

\arguments{
  \item{fname}{file name of text file containing the list}
  \item{x}{list to write out}
  \item{format}{format of file to create. \dQuote{P} or \dQuote{D}}
  \item{comments}{vector of character strings to be used as initial comments in the file}
}

\details{
  The \dQuote{D} format is equivalent to using dput and dget, which supports all R objects.
  
  The \dQuote{P} format only supports named lists of vectors, matrices, and data-frames.
  Nested lists are not supported. In the simplest form, the \dQuote{P} format consists of a 
  named list element prefixed with a dollar sign (\$) on a single line with data on the 
  following line(s). All following data will be until a new variable name is found 
  (denoted by \$), or the end of a file is reached.
  
  If multiple lines of data are used, then the data is treated as a matrix, and all rows must
  have the same amount of values (separated by whitespace).
  
  It is possible to specify more advanced options by including a line with two dollar signs (\$\$)
  that follows on the next immediate line after the variable name declaration. These options allow
  names for vectors, and colnames, as well as data.frames objects. For complete details see the PBS 
  modelling PDF.
}

\note{
  \dQuote{P} format only supports a list of vectors, or matrices, but cannot support sub-lists.
  However \dQuote{D} format supports all R objects
}

\seealso{
  \code{\link{readList}}

  \code{\link{dput}}
}

\keyword{file}
\keyword{list}

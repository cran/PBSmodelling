\name{writeList}
\alias{writeList}
\title{Write a List to a File in PBS Modelling Format}

\description{
	Write an ASCII text representation in either \code{"D"} format or \code{"P"} format. 
	The \code{"D"} format makes use of \code{dput} and \code{dget}, and produces an R 
	representation of the list. The \code{"P"} format represents a simple list in an 
	easy-to-read, ad hoc \code{PBSmodelling} format.
}
\usage{
writeList(x, fname, format="D", comments="")
}
\arguments{
	\item{x}{R list object to write to an ASCII text file.}
	\item{fname}{file name of the text file containing the list.}
	\item{format}{format of the file to create: \code{"D"} or \code{"P"}.}
	\item{comments}{vector of character strings to use as initial-line comments in the file.}
}
\details{
	The \code{"D"} format is equivalent to using R's \code{base} functions 
	\code{dput} and \code{dget}, which support all R objects.

	The \code{"P"} format only supports named lists of vectors, matrices, and data frames. 
	Scalars are treated like vectors. Nested lists are not supported. 

	The \code{"P"} format writes each named element in a list using the 
	following conventions: (i) \$ followed by the name of the data object 
	to denote the start of that object's description; (ii) \$\$ on the next line 
	to describe the object's structure - object type, mode(s), names (if vector), 
	rownames (if matrix or data), and colnames (if matrix or data); and 
	(iii) subsequent lines of data (one line for vector, multiple lines for matrix or data).

	Multiple rows of data for matrices or data frames must have equal 
	numbers of entries (separated by whitespace).

	For complete details, see \dQuote{PBSmodelling-UG.pdf} in R's 
	directory \code{library/PBSmodelling}.
}
\seealso{
	\code{\link{readList}}, \code{\link{openFile}}
}
\examples{
\dontrun{
test <- list(a=10,b=euro,c=view(WorldPhones),d=view(USArrests))
writeList(test,"test.txt",format="P",
	comments=" Scalar, Vector, Matrix, Data Frame")
openFile("test.txt")
}
}
\keyword{file}
\keyword{list}

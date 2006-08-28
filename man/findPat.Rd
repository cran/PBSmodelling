\name{findPat}
\alias{findPat}
\title{Search a Vector With Multiple Patterns}

\description{
  Searches all patterns in pat from vec, and returns the
  matched elements in vec.
}

\usage{
findPat(pat, vec) 
}

\arguments{
  \item{pat}{character vector of patterns to match in vec}
  \item{vec}{character vector where matches are sought}
}

\value{
  A character vector of all matched strings.
}

\examples{
#find all strings with a vowel, or that start with a number
findPat(c("[aeoiy]", "^[0-9]"), c("hello", "WRLD", "11b"))
}

\keyword{utilities}

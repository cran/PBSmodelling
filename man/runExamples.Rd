\name{runExamples}
\alias{runExamples}
\title{Run GUI Examples Included with PBS Modelling}

\description{
	Display an interactive GUI to demonstrate PBS Modelling examples.

	The example source files can be found in the directory \code{PBSmodelling/examples}, 
	located in R's directory \code{library}.
}
\usage{runExamples()}

\details{
	Some examples use external packages which must be installed to work correctly:

	\code{BRugs} - \code{LinReg}, \code{MarkRec}, and \code{CCA};

	\code{odesolve/ddesolve} - \code{FishRes};

	\code{PBSmapping} - \code{FishTows}.
}
\note{
	The examples are copied from \code{PBSmodelling/examples} to R's current 
	temporary working directory and run from there.
}
\seealso{
	\code{\link{runDemos}}
}
\keyword{utilities}

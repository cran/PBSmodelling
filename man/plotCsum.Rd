\name{plotCsum}
\alias{plotCsum}
\title{Plots Cumulative Frequency of Data}

\description{
  Plots cumulative frequency of data
}

\usage{
plotCsum(x, add = FALSE, ylim = c(0, 1), xlab = "Measure", 
ylab = "Cumulative Proportion", ...)  
}

\arguments{
  \item{x}{vector of values}
  \item{add}{if TRUE, add cumulative frequency curve to current plot}
  \item{ylim}{limits for y-axis}
  \item{xlab}{label for x-axis}
  \item{ylab}{label for y-axis}
  \item{...}{additional arguments for plot.}
}

\examples{
x <- rgamma(n=1000,shape=2)
plotCsum(x)
}

\keyword{hplot}

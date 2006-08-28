\name{createWin}
\alias{createWin}
\title{Create a GUI Window}

\description{
  The createWin function takes a window markup file, and creates a window based on the markup file.
}

\usage{createWin(fname, astext=FALSE)}

\arguments{
  \item{fname}{filename of markup file or list returned from \link{parseWinFile}.}
  \item{astext}{if true, fname is intrupted as a vector of strings. with each element representing a line of the source of a window description file}
}

\details{
  The markup file contains a single widget per line. Widgets can span multiple lines by including a 
  backslash ('\\') as the last character of the line, which then ignores the newline.

  For more details about widget types, and the markup file see the pdf located in the installation
  directory.

  It is possible to use a Window Description List produced by compileDescription rather than a filename for the fname argument.

  Another alternative is to set astext=TRUE and pass in a vector of characters for fname. This vector of characters represents the file contents.
  Each element of the vector is equivalent to a new line in the window description file.
}

\value{
  PBS.win contains window information such as: present values, present variable names, present
  variable values, and triggered action values. This information is encapsulated in a list which is 
  set as the global PBS.win variable, which is also returned.

  PBS.win
  \item{vars}{Current widget values}
  \item{funs}{Functions required by Window}
  \item{names}{Variable names: names(PBS.win\$vars)}
  \item{action}{Action that triggered a function call}
}

\seealso{
  \code{\link{parseWinFile}}
  
  \code{\link{getWinVal}}
  
  \code{\link{setWinVal}}
  
  \code{\link{closeWin}}
  
  \code{\link{compileDescription}}
  
  \code{\link{createVector}}
  
  \code{\link{initPBShistory}} for an example of using astext=TRUE
}

\examples{

#see file testWidgets\LissWin.txt in PBSmodelling package directory
#
# window title="Lissajous Curve"
# grid 1 2
#   label text=Pars: font=bold
#   vector length=4 names="m n phi k" \
#     labels="'x cycles' 'y cycles' 'y phase' points" \
#     values="2 3 0 1000" vertical=T
# grid 1 2
#   label text=History: font=bold
#   history
# grid 1 2
#   grid 2 1
#     radio name=ptype text=lines  value="l" mode=character
#     radio name=ptype text=points value="p" mode=character
# button text=Plot func=drawLiss font=bold
#

# Calculate and draw the Lissajous figure
drawLiss <- function()
{
  getWinVal(scope="L");
  ti <- 2*pi*(0:k)/k;
  x <- sin(2*pi*m*ti);
  y <- sin(2*pi*(n*ti+phi));

  plot(x,y,type=ptype);
  invisible(NULL);
}
\dontrun{
require(PBSmodelling); 
createWin(system.file("testWidgets/LissWin.txt",package="PBSmodelling"))
}
}

\keyword{utilities}

# Script to create Appendix C tex file "AppC.tex"
# Assume that:
# 1. The current directory contains AppChead.tex and AppCfoot.tex
# 2. The subdirectory .\Rhelp contains all LaTeX help files for the package

makeApp <- function() {

  # Get all files ending in ".tex"
  texFiles <- sort(grep("\\.tex$", dir("Rhelp"), value=T));
  texFiles <- paste("\\input{Rhelp/", texFiles, "}", sep="");
  texInput <- paste(texFiles, collapse="\n");

  # Read the header and footer files
  texH <- scan("AppChead.tex",what="x",sep="\n",blank=F);
  texF <- scan("AppCfoot.tex",what="x",sep="\n",blank=F);
  texHead <- paste(texH, collapse="\n");
  texFoot <- paste(texF, collapse="\n");

  # Open the output file, write output, and close
  sink(file="AppC.tex");
  cat(texHead); cat("\n\n");
  cat(texInput); cat("\n\n");
  cat(texFoot); cat("\n");
  sink()
}

makeApp();
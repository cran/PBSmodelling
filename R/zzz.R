
.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmodelling", pkg, lib)
	.initPBSoptions()
	cat("
PBS Modelling 1.59 -- Copyright (C) 2005-2007 Fisheries and Oceans Canada

A complete user guide, which contains much more than the help files, appears as
PBSmodelling-UG.pdf in the root library directory of PBSmodelling. To use this
package effectively, please consult the guide.

Built on Aug 30, 2007
Pacific Biological Station, Nanaimo

Type runDemos() to see all available demos on your system.
Type runExamples() to see the current examples in PBSmodelling.
")
}

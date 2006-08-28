
.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmodelling", pkg, lib)
	.initPBSoptions()
	cat("
PBS Modelling 0.60 -- Copyright (C) 2005-2006 Fisheries and Oceans Canada

A complete user guide, which contains much more than the help files, appears as
PBSmodelling-UG.pdf in the root library directory of PBSmodelling. To use this
package effectively, please consult the guide.

Last built on Tue Aug 28, 2006 (13:00:00)
")
}

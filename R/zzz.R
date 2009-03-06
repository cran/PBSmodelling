
.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmodelling", pkg, lib)
	.initPBSoptions()
	cat("
PBS Modelling 2.06 -- Copyright (C) 2005-2009 Fisheries and Oceans Canada

A complete user guide 'PBSmodelling-UG.pdf' appears 
in the '.../library/PBSmodelling/doc' folder.

Built on Mar 6, 2009
Pacific Biological Station, Nanaimo

")
}

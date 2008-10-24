
.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmodelling", pkg, lib)
	.initPBSoptions()
	cat("
PBS Modelling 2.01 -- Copyright (C) 2005-2008 Fisheries and Oceans Canada

A complete user guide 'PBSmodelling-UG.pdf' appears 
in the '.../library/PBSmodelling/doc' folder.

Built on Oct 24, 2008
Pacific Biological Station, Nanaimo

")
}

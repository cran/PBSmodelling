
.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmodelling", pkg, lib)
	.initPBSoptions()
	cat("
PBS Modelling 1.63 -- Copyright (C) 2005-2008 Fisheries and Oceans Canada

A complete user guide 'PBSmodelling-UG.pdf' appears 
in the '../library/PBSmodelling/doc' folder.

Built on May 29, 2008
Pacific Biological Station, Nanaimo

")
}

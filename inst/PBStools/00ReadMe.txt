The batch files in this directory can facilitate R package development 
under Windows. For more information on building packages, see apendix B 
of the PBS Modelling User Guide*.

Revised batch files are included here in addition to the originals 
(original -> revised):
------------------------------
definePaths.bat -> RPaths.bat
checkPaths. bat -> RPathCheck.bat
check.bat       -> Rcheck.bat
build.bat       -> Rbuild.bat
pack.bat        -> Rpack.bat
unpack.bat      -> Runpack.bat
makePDF.bat     -> RmakePDF.bat
makePDF2.bat    -> RmakePDF2.bat

You can use the small package archived in PBStry_x.xx.tar.gz as a 
convenient prototype for starting your own new package. It includes 
functions with C code called by the R function .C().
-------------------------------------------------------------------------
*Note: Please remember to consult the User's Guide contained in the file
  ...\library\PBSmodelling\PBSmodelling-UG.pdf,
where "..." denotes the path to your R installation.

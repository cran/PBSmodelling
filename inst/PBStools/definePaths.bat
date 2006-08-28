@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the six PATH variables listed below *****

set R_PATH=C:\Utils\R\R-2.3.1\bin
set TOOLS_PATH=C:\Utils\Rtools\bin
set PERL_PATH=C:\Utils\Perl\bin
set MINGW_PATH=C:\Utils\MinGW\bin
set TEX_PATH=C:\Utils\MiKTeX\miktex\bin
set HTMLHELP_PATH=C:\Utils\HHW
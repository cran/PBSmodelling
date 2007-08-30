@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the six PATH variables listed below *****

set R_Root=C:\WinApps\R\R-2.5.1
set TOOLS_PATH=C:\Utils\Rtools\bin
set PERL_PATH=C:\WinApps\Perl\bin
set MINGW_PATH=C:\Utils\Rtools\MinGW\bin
set TEX_PATH=C:\WinApps\MiKTeX\miktex\bin
set HTMLHELP_PATH=C:\Utils\HHW

set R_PATH=%R_Root%\bin

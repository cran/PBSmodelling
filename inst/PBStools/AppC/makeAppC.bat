@Echo off

setlocal

Rem ***** Set these paths, similar to definePaths.bat *****
set RBASE=C:\Utils\R\R-2.3.1
set RTOOLS=C:\Utils\Rtools\bin
set PERL=C:\Utils\Perl\bin
set TEX=C:\Utils\MiKTeX\miktex\bin

set PATH=.;%RTOOLS%;%RBASE%\bin;%PERL%;%TEX%
Echo %Path%

mkdir Rhelp
del /Q Rhelp\*

cd Rhelp
unzip "%RBASE%\library\PBSmodelling\latex\Rhelp.zip"
cd ..

R CMD BATCH --vanilla makeAppC.r makeAppC.log
pdflatex AppC.tex

pause

endlocal
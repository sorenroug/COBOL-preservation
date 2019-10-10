@echo off
cls
if exist %1.obj  del %1.obj >nul
if exist %1.exe  del %1.exe >nul
echo Program is compiled.  Please,wait for the compilation.
echo ÿ
cobol %1,%1,%1,nul
if not exist %1.obj  goto end1
cls
echo Program is linked.  Please,wait for the linking.
link  %1,%1,nul,,nul
if not exist %1.exe  goto end
if     exist %1.obj  del %1.obj >nul
if     exist %1.lst  del %1.lst >nul
cls
%1
echo ########################################ÿ
echo If it have run time error, it is not OK.
echo ########################################
pause
goto end
:end1
type %1.lst
echo Error on your source.
:end

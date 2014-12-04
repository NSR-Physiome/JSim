@echo off

set DIR=%~d0%~p0
set REG="%DIR%reg.exe"
set PROG=jsim.bat
set APP=JSim
set ICON=%DIR%..\lib\JSim.ico
set FILETYPE=proj
set EXT=proj mod tac jsml

echo   This command makes additions to the Windows
echo Registry that cause Windows Explorer to launch
echo the JSim version in %DIR%
echo when you double-click on files with any of
echo the following extensions: %EXT%
echo   This may potentially cause problems for other
echo programs on you system if they use those same
echo extensions.
echo   Also beware that this program has, so far,  only been
echo tested under Windows XP and Windows 7. mod file association
echo does not work as .mod is used for media files.
echo   Hit Control-C to terminate this job now if your
echo are not sure this is what you want to do.
pause

rem Set up hkcr/JSim

set KEY=hkcr\%APP%
rem %REG% delete %KEY% /f
%REG% add %KEY% /f

set KEY=hkcr\%APP%\%FILETYPE%
%REG% add %KEY% /ve /t REG_SZ /d %APP%\%FILETYPE% /f

set KEY1=%KEY%\DefaultIcon
%REG% add %KEY1% /ve /t REG_SZ /d "\"%ICON%\"" /f

set KEY=%KEY%\shell
%REG% add %KEY% /ve /t REG_SZ /d Open /f

set KEY=%KEY%\Open
%REG% add %KEY% /f

set KEY=%KEY%\command
%REG% add %KEY% /ve /t REG_SZ /d "\"%DIR%%PROG%\" \"-f\" \"%%1\"" /f 

rem Set up all file extensions

for %%e in ( %EXT% ) do %REG% add hkcr\.%%e /ve /t REG_SZ /d %APP%\%FILETYPE% /f


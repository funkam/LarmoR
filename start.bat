cd /d "%~dp0"

setlocal EnableDelayedExpansion
title LarmoR
color 0B

echo.
echo  ===============================================================
echo    LarmoR
echo  ===============================================================
echo.

REM -- Locate R: cached path first, then registry, then folders --
set "RSCRIPT="

if exist "%~dp0.rpath" (
    set /p RSCRIPT=<"%~dp0.rpath"
    if not exist "!RSCRIPT!" set "RSCRIPT="
)

if not defined RSCRIPT (
    call :reg_lookup "HKLM\SOFTWARE\R-core\R"
)
if not defined RSCRIPT (
    call :reg_lookup "HKCU\SOFTWARE\R-core\R"
)
if not defined RSCRIPT (
    call :scan_dir "%ProgramFiles%\R"
)
if not defined RSCRIPT (
    call :scan_dir "%LOCALAPPDATA%\Programs\R"
)

if not defined RSCRIPT (
    color 0C
    echo  R was not found.
    echo.
    echo  Please run install.bat first.
    echo.
    pause
    exit /b 1
)

if not exist "%~dp0app.R" (
    color 0C
    echo  app.R not found in this folder.
    echo.
    pause
    exit /b 1
)

echo  Starting server...
echo.
echo  The app will open in your browser shortly.
echo.
echo  ---------------------------------------------------------------
echo   KEEP THIS WINDOW OPEN while using LarmoR.
echo   Close it to shut the app down.
echo  ---------------------------------------------------------------
echo.

"!RSCRIPT!" --vanilla "%~dp0run_app.R"
set "EXITCODE=!errorlevel!"

if !EXITCODE! NEQ 0 (
    color 0C
    echo.
    echo  ---------------------------------------------------------------
    echo   The app stopped with an error.
    echo   See error_log.txt in this folder for details.
    echo  ---------------------------------------------------------------
    echo.
    pause
)

endlocal
exit /b %EXITCODE%

REM ===============================================================
:reg_lookup
for /f "tokens=2,*" %%A in ('reg query %1 /v InstallPath 2^>nul ^| find "InstallPath"') do (
    if exist "%%B\bin\Rscript.exe" set "RSCRIPT=%%B\bin\Rscript.exe"
)
exit /b

:scan_dir
if not exist "%~1" exit /b
for /f "delims=" %%V in ('dir /b /ad /o-n "%~1\R-*" 2^>nul') do (
    if exist "%~1\%%V\bin\Rscript.exe" (
        set "RSCRIPT=%~1\%%V\bin\Rscript.exe"
        exit /b
    )
)
exit /b

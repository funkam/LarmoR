start_bat <- 'cd /d "%~dp0"

setlocal EnableDelayedExpansion
title IVDrDataManager
color 0B

echo.
echo  ===============================================================
echo    IVDrDataManager
echo  ===============================================================
echo.

REM ---------------------------------------------------------------
REM  Locate R - use cached path from installer if available
REM ---------------------------------------------------------------
set "RSCRIPT="

if exist "%~dp0.rpath" (
    set /p RSCRIPT=<"%~dp0.rpath"
    if not exist "!RSCRIPT!" set "RSCRIPT="
)

if not defined RSCRIPT (
    where Rscript.exe >nul 2>&1
    if !errorlevel! equ 0 (
        for /f "delims=" %%i in (\'where Rscript.exe\') do (
            set "RSCRIPT=%%i"
            goto :got_r
        )
    )
    for %%D in ("%ProgramFiles%\\R" "%ProgramFiles(x86)%\\R" "%LOCALAPPDATA%\\Programs\\R") do (
        if exist "%%~D" (
            for /f "delims=" %%V in (\'dir /b /ad /o-n "%%~D\\R-*" 2^>nul\') do (
                if exist "%%~D\\%%V\\bin\\Rscript.exe" (
                    set "RSCRIPT=%%~D\\%%V\\bin\\Rscript.exe"
                    goto :got_r
                )
            )
        )
    )
)

:got_r
if not defined RSCRIPT (
    color 0C
    echo  R was not found.
    echo.
    echo  Please run install.bat first.
    echo.
    pause
    exit /b 1
)

REM ---------------------------------------------------------------
REM  Sanity checks
REM ---------------------------------------------------------------
if not exist "%~dp0app.R" (
    color 0C
    echo  app.R not found in this folder.
    echo.
    pause
    exit /b 1
)

if not exist "%~dp0config.R" (
    if exist "%~dp0config.example.R" (
        copy "%~dp0config.example.R" "%~dp0config.R" >nul
        echo  Created config.R from template.
        echo.
    )
)

REM ---------------------------------------------------------------
REM  Launch
REM ---------------------------------------------------------------
echo  Starting server...
echo.
echo  The app will open in your browser shortly.
echo.
echo  ---------------------------------------------------------------
echo   KEEP THIS WINDOW OPEN while using the app.
echo   Close it to shut the app down.
echo  ---------------------------------------------------------------
echo.

"!RSCRIPT!" --vanilla "%~dp0run_app.R"

set "EXITCODE=!errorlevel!"

if !EXITCODE! neq 0 (
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
exit /b 0
'

writeLines(start_bat, "start.bat", sep = "\r\n")
cat("Wrote start.bat\n")

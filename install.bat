cd /d "%~dp0"

setlocal EnableDelayedExpansion
title LarmoR - Installer
color 0B

echo.
echo  ===============================================================
echo    LarmoR - Installation
echo  ===============================================================
echo.

REM ---------------------------------------------------------------
REM  1. Locate R
REM ---------------------------------------------------------------
echo  [1/3] Looking for R...

set "RSCRIPT="

REM -- Manual override: uncomment and set your path if needed --
REM set "RSCRIPT=C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
if defined RSCRIPT (
    if exist "!RSCRIPT!" goto :found_r
    set "RSCRIPT="
)

REM -- 1a. PATH --
for /f "delims=" %%i in ('where Rscript.exe 2^>nul') do (
    set "RSCRIPT=%%i"
    goto :found_r
)

REM -- 1b. Registry --
call :reg_lookup "HKLM\SOFTWARE\R-core\R"
if defined RSCRIPT goto :found_r
call :reg_lookup "HKLM\SOFTWARE\WOW6432Node\R-core\R"
if defined RSCRIPT goto :found_r
call :reg_lookup "HKCU\SOFTWARE\R-core\R"
if defined RSCRIPT goto :found_r

REM -- 1c. Standard folders --
call :scan_dir "%ProgramFiles%\R"
if defined RSCRIPT goto :found_r
call :scan_dir "%LOCALAPPDATA%\Programs\R"
if defined RSCRIPT goto :found_r
set "PF86=%ProgramFiles(x86)%"
call :scan_dir "!PF86!\R"
if defined RSCRIPT goto :found_r

goto :no_r

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

REM ===============================================================
:no_r
color 0E
echo.
echo  R was not found on this computer.
echo.
echo  LarmoR needs R version 4.2 or newer.
echo.
echo  Please install R from:
echo    https://cran.r-project.org/bin/windows/base/
echo.
echo  If R is already installed in an unusual location, open
echo  install.bat in a text editor and set RSCRIPT near the top.
echo.
start "" "https://cran.r-project.org/bin/windows/base/"
pause
exit /b 1

REM ===============================================================
:found_r
color 0B
echo        Found: !RSCRIPT!


> "%~dp0.rpath" echo !RSCRIPT!

REM ---------------------------------------------------------------
REM  2. Install R packages
REM ---------------------------------------------------------------
echo.
echo  [2/3] Installing R packages...
echo        First run may take 5-15 minutes.
echo.

if not exist "%~dp0setup_packages.R" (
    color 0C
    echo  ERROR: setup_packages.R not found.
    pause
    exit /b 1
)

"!RSCRIPT!" --vanilla "%~dp0setup_packages.R"

if !errorlevel! neq 0 (
    color 0C
    echo.
    echo  Package installation failed. See messages above.
    echo.
    pause
    exit /b 1
)

REM ---------------------------------------------------------------
REM  3. Desktop shortcut
REM ---------------------------------------------------------------
echo.
echo  [3/3] Creating desktop shortcut...

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ws = New-Object -ComObject WScript.Shell; " ^
  "$sc = $ws.CreateShortcut([Environment]::GetFolderPath('Desktop') + '\LarmoR.lnk'); " ^
  "$sc.TargetPath = '%~dp0start.bat'; " ^
  "$sc.WorkingDirectory = '%~dp0'; " ^
  "$sc.Description = 'LarmoR - NMR Sample and Data Manager'; " ^
  "if (Test-Path '%~dp0www\icon.ico') { $sc.IconLocation = '%~dp0www\icon.ico' }; " ^
  "$sc.Save()" >nul 2>&1

if exist "%USERPROFILE%\Desktop\LarmoR.lnk" (
    echo        Shortcut created on Desktop.
) else (
    echo        Could not create shortcut - use start.bat instead.
)

color 0A
echo.
echo  ===============================================================
echo    Installation complete
echo  ===============================================================
echo.
echo    Start LarmoR by double-clicking the Desktop shortcut
echo    or start.bat in this folder.
echo.
echo    A setup wizard will run on first launch.
echo.
echo  ===============================================================
echo.

choice /c YN /m "  Start LarmoR now"
if errorlevel 2 goto :done
start "" "%~dp0start.bat"

:done
endlocal
exit /b 0

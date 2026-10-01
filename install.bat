install_bat <- 'cd /d "%~dp0"

setlocal EnableDelayedExpansion
title IVDrDataManager - Installer
color 0B

echo.
echo  ===============================================================
echo    IVDrDataManager - Installation
echo  ===============================================================
echo.

REM ---------------------------------------------------------------
REM  1. Locate R
REM ---------------------------------------------------------------
echo  [1/4] Looking for R...

set "RSCRIPT="

REM Try PATH first
where Rscript.exe >nul 2>&1
if !errorlevel! equ 0 (
    for /f "delims=" %%i in (\'where Rscript.exe\') do (
        set "RSCRIPT=%%i"
        goto :found_r
    )
)

REM Try standard install locations, newest version wins
for %%D in ("%ProgramFiles%\\R" "%ProgramFiles(x86)%\\R" "%LOCALAPPDATA%\\Programs\\R") do (
    if exist "%%~D" (
        for /f "delims=" %%V in (\'dir /b /ad /o-n "%%~D\\R-*" 2^>nul\') do (
            if exist "%%~D\\%%V\\bin\\Rscript.exe" (
                set "RSCRIPT=%%~D\\%%V\\bin\\Rscript.exe"
                goto :found_r
            )
        )
    )
)

REM Try registry
for /f "tokens=2,*" %%A in (\'reg query "HKLM\\SOFTWARE\\R-core\\R" /v InstallPath 2^>nul ^| find "InstallPath"\') do (
    if exist "%%B\\bin\\Rscript.exe" (
        set "RSCRIPT=%%B\\bin\\Rscript.exe"
        goto :found_r
    )
)

REM ---------------------------------------------------------------
REM  R not found - offer to install
REM ---------------------------------------------------------------
color 0E
echo.
echo  R was not found on this computer.
echo.
echo  IVDrDataManager needs R version 4.2 or newer.
echo.
choice /c YN /m "  Download and install R now"
if errorlevel 2 goto :manual_r

echo.
echo  Downloading R installer (about 80 MB)...
set "RURL=https://cran.r-project.org/bin/windows/base/release.html"
set "RTMP=%TEMP%\\R-installer.exe"

powershell -NoProfile -Command ^
  "$ProgressPreference=\'SilentlyContinue\'; " ^
  "$page = Invoke-WebRequest -Uri \'%RURL%\' -UseBasicParsing; " ^
  "$link = ($page.Links | Where-Object { $_.href -match \'R-[\\d.]+-win\\.exe\' } | Select-Object -First 1).href; " ^
  "if (-not $link) { $link = \'https://cran.r-project.org/bin/windows/base/R-4.4.2-win.exe\' } " ^
  "if ($link -notmatch \'^https?://\') { $link = \'https://cran.r-project.org/bin/windows/base/\' + $link }; " ^
  "Write-Host \'  Source:\' $link; " ^
  "Invoke-WebRequest -Uri $link -OutFile \'%RTMP%\'"

if not exist "%RTMP%" (
    color 0C
    echo.
    echo  Download failed. Please install R manually.
    goto :manual_r
)

echo.
echo  Running R installer. Accept the defaults when prompted.
echo.
start /wait "" "%RTMP%" /SILENT /NORESTART
del "%RTMP%" >nul 2>&1

REM Re-scan after install
for %%D in ("%ProgramFiles%\\R" "%LOCALAPPDATA%\\Programs\\R") do (
    if exist "%%~D" (
        for /f "delims=" %%V in (\'dir /b /ad /o-n "%%~D\\R-*" 2^>nul\') do (
            if exist "%%~D\\%%V\\bin\\Rscript.exe" (
                set "RSCRIPT=%%~D\\%%V\\bin\\Rscript.exe"
                goto :found_r
            )
        )
    )
)

:manual_r
color 0C
echo.
echo  ---------------------------------------------------------------
echo   Please install R manually, then run this installer again.
echo.
echo     https://cran.r-project.org/bin/windows/base/
echo  ---------------------------------------------------------------
echo.
start "" "https://cran.r-project.org/bin/windows/base/"
pause
exit /b 1

:found_r
color 0B
echo        Found: !RSCRIPT!

REM Remember the path so the launcher does not have to search again
echo !RSCRIPT!> "%~dp0.rpath"

REM ---------------------------------------------------------------
REM  2. Install R packages
REM ---------------------------------------------------------------
echo.
echo  [2/4] Installing R packages...
echo        First run may take 5-15 minutes. Please be patient.
echo.

"!RSCRIPT!" --vanilla "%~dp0setup_packages.R"

if !errorlevel! neq 0 (
    color 0C
    echo.
    echo  Package installation failed. See messages above.
    pause
    exit /b 1
)

REM ---------------------------------------------------------------
REM  3. Create configuration
REM ---------------------------------------------------------------
echo.
echo  [3/4] Setting up configuration...

if exist "%~dp0config.R" (
    echo        config.R already exists - keeping your settings.
) else (
    if exist "%~dp0config.example.R" (
        copy "%~dp0config.example.R" "%~dp0config.R" >nul
        echo        Created config.R from template.
    )
)

REM ---------------------------------------------------------------
REM  4. Desktop shortcut
REM ---------------------------------------------------------------
echo.
echo  [4/4] Creating desktop shortcut...

powershell -NoProfile -Command ^
  "$ws = New-Object -ComObject WScript.Shell; " ^
  "$sc = $ws.CreateShortcut([Environment]::GetFolderPath(\'Desktop\') + \'\\IVDrDataManager.lnk\'); " ^
  "$sc.TargetPath = \'%~dp0start.bat\'; " ^
  "$sc.WorkingDirectory = \'%~dp0\'; " ^
  "$sc.Description = \'IVDr NMR Data Manager\'; " ^
  "if (Test-Path \'%~dp0www\\icon.ico\') { $sc.IconLocation = \'%~dp0www\\icon.ico\' }; " ^
  "$sc.Save()" >nul 2>&1

if exist "%USERPROFILE%\\Desktop\\IVDrDataManager.lnk" (
    echo        Shortcut created on Desktop.
) else (
    echo        Could not create shortcut - use start.bat instead.
)

REM ---------------------------------------------------------------
color 0A
echo.
echo  ===============================================================
echo    Installation complete
echo  ===============================================================
echo.
echo    Start the app by:
echo      - double-clicking IVDrDataManager on your Desktop, or
echo      - double-clicking start.bat in this folder
echo.
echo    Optional: edit config.R to set your lab name and data folder.
echo.
echo  ===============================================================
echo.

choice /c YN /m "  Start IVDrDataManager now"
if errorlevel 2 goto :done
start "" "%~dp0start.bat"

:done
endlocal
exit /b 0
'

writeLines(install_bat, "install.bat", sep = "\r\n")
cat("Wrote install.bat\n")

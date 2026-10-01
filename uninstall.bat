uninstall_bat <- 'cd /d "%~dp0"
title IVDrDataManager - Uninstall
color 0E

echo.
echo  ===============================================================
echo    IVDrDataManager - Uninstall
echo  ===============================================================
echo.
echo  This removes the desktop shortcut and cached settings.
echo.
echo  It does NOT remove:
echo    - R itself
echo    - your data files (CSV, Excel)
echo    - this application folder
echo.

choice /c YN /m "  Continue"
if errorlevel 2 exit /b 0

if exist "%USERPROFILE%\\Desktop\\IVDrDataManager.lnk" (
    del "%USERPROFILE%\\Desktop\\IVDrDataManager.lnk"
    echo  Removed desktop shortcut.
)

if exist "%~dp0.rpath" del "%~dp0.rpath"
if exist "%~dp0error_log.txt" del "%~dp0error_log.txt"

echo.
echo  Done. Delete this folder manually to finish removing the app.
echo.
pause
exit /b 0
'

writeLines(uninstall_bat, "uninstall.bat", sep = "\r\n")
cat("Wrote uninstall.bat\n")

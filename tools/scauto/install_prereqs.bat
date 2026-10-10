@echo off
setlocal EnableExtensions
REM Install R if it is missing. Analysis packages install when you click Start analysis.
REM Double-click this file, or Launch.bat will offer it when R is not found.
cd /d "%~dp0..\.."
if errorlevel 1 (
  echo Could not find the project folder.
  pause
  exit /b 1
)

echo ===== PBMC Auto-Pilot: install R =====
echo Project: %CD%
echo Analysis packages download later when you click Start analysis.
echo.

set "RSCRIPT="
call :find_rscript
if defined RSCRIPT goto :have_r

echo R is not installed on this computer.
where winget >nul 2>&1
if %errorlevel%==0 (
  echo Installing R with Windows Package Manager (winget)...
  winget install --id RProject.R --accept-package-agreements --accept-source-agreements
  call :find_rscript
)

if not defined RSCRIPT (
  echo.
  echo Opening the official R download page.
  echo Install R 4.3 or newer. Tick "Add R to PATH" if you see that option.
  start "" "https://cran.r-project.org/bin/windows/base/"
  echo.
  pause
  call :find_rscript
)

if not defined RSCRIPT (
  echo Still cannot find Rscript.exe
  echo Install R from https://cran.r-project.org then run this file again.
  echo See tools\scauto\INSTALL.md
  pause
  exit /b 1
)

:have_r
echo Using: %RSCRIPT%
echo.
echo R is ready. Open the helper; click Start analysis to install Seurat if needed.
echo Next: double-click 2_Start.bat  (or tools\scauto\ui\Launch.bat)
pause
exit /b 0

:find_rscript
set "RSCRIPT="
where Rscript >nul 2>&1
if %errorlevel%==0 (
  for /f "delims=" %%I in ('where Rscript') do (
    set "RSCRIPT=%%I"
    goto :eof
  )
)
if defined ProgramFiles (
  for /d %%D in ("%ProgramFiles%\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" (
      set "RSCRIPT=%%D\bin\Rscript.exe"
      goto :eof
    )
    if exist "%%D\bin\x64\Rscript.exe" (
      set "RSCRIPT=%%D\bin\x64\Rscript.exe"
      goto :eof
    )
  )
)
if defined ProgramFiles(x86) (
  for /d %%D in ("%ProgramFiles(x86)%\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" (
      set "RSCRIPT=%%D\bin\Rscript.exe"
      goto :eof
    )
    if exist "%%D\bin\x64\Rscript.exe" (
      set "RSCRIPT=%%D\bin\x64\Rscript.exe"
      goto :eof
    )
  )
)
goto :eof

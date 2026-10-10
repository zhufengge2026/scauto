@echo off
setlocal EnableExtensions
REM Double-click to open PBMC Auto-Pilot (Windows).
cd /d "%~dp0..\..\.."
if errorlevel 1 (
  echo Could not find the project folder.
  pause
  exit /b 1
)

set "LAUNCH=%~dp0launch.R"
if not exist "%LAUNCH%" (
  echo Missing "%LAUNCH%"
  pause
  exit /b 1
)

set "RSCRIPT="
where Rscript >nul 2>&1
if %errorlevel%==0 (
  for /f "delims=" %%I in ('where Rscript') do (
    set "RSCRIPT=%%I"
    goto :run
  )
)

if defined ProgramFiles (
  for /d %%D in ("%ProgramFiles%\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" (
      set "RSCRIPT=%%D\bin\Rscript.exe"
      goto :run
    )
    if exist "%%D\bin\x64\Rscript.exe" (
      set "RSCRIPT=%%D\bin\x64\Rscript.exe"
      goto :run
    )
  )
)
if defined ProgramFiles(x86) (
  for /d %%D in ("%ProgramFiles(x86)%\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" (
      set "RSCRIPT=%%D\bin\Rscript.exe"
      goto :run
    )
    if exist "%%D\bin\x64\Rscript.exe" (
      set "RSCRIPT=%%D\bin\x64\Rscript.exe"
      goto :run
    )
  )
)

echo Rscript was not found. R is not installed on this computer.
echo.
echo Running the installer (needs internet, several minutes)...
echo See tools\scauto\INSTALL.md
call "%~dp0..\install_prereqs.bat"
call :find_rscript_after
if not defined RSCRIPT (
  echo Still no R. Install it from https://cran.r-project.org then try Launch.bat again.
  pause
  exit /b 1
)
goto :run

:find_rscript_after
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
    if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe" & goto :eof
    if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT=%%D\bin\x64\Rscript.exe" & goto :eof
  )
)
goto :eof

:run
echo Starting PBMC Auto-Pilot
echo Browser: http://127.0.0.1:8765
echo Closing this window stops the app.
echo Using: %RSCRIPT%
"%RSCRIPT%" "%LAUNCH%"
if errorlevel 1 (
  echo.
  echo The helper did not start. Stay online the first time (it installs helper packages).
  echo See tools\scauto\INSTALL.md
  pause
)
endlocal

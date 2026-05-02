@echo off
cd /d "%~dp0"

REM === Read elm path ===
set ELM=
set /p ELM=<elm_path.txt
if not defined ELM goto :no_elm

REM === Build ===
echo ========================================
echo  Hex-Studio Build
echo ========================================
echo.
echo [Build] Compiling...
"%ELM%" make src\Main.elm --output=gen\source.js
if %errorlevel% neq 0 goto :fail
echo [OK] Build success!
echo.

REM === Verify output exists ===
if not exist "gen\source.js" (
    echo [ERROR] gen\source.js was not generated
    pause
    exit /b 1
)

REM === Start HTTP server ===
set PORT=8080
set URL=http://localhost:%PORT%

echo ========================================
echo  Hex-Studio Server
echo ========================================
echo.
echo [Server] http://localhost:%PORT%
echo [Server] Press Ctrl+C to stop
echo.

REM Kill existing process on port
for /f "tokens=5" %%a in ('netstat -ano ^| findstr ":%PORT% "') do (
    taskkill /f /pid %%a >nul 2>&1
)

REM Try Python (no --directory flag, we already cd'd)
where python >nul 2>&1
if %errorlevel% equ 0 (
    start "" "%URL%"
    python -m http.server %PORT%
    goto :eof
)

REM Try Node
where node >nul 2>&1
if %errorlevel% equ 0 (
    start "" "%URL%"
    npx --yes serve . --listen %PORT% --no-clipboard
    goto :eof
)

REM Fallback
echo [WARN] Neither Python nor Node.js found.
echo Install Python or Node.js for local server.
echo Opening file:// instead (may not work)...
start "" "index.html"
goto :eof

:fail
echo.
echo ========================================
echo  Build FAILED!
echo ========================================
pause
exit /b 1

:no_elm
echo [ERROR] elm_path.txt is empty or missing.
echo.
echo 1. Download Elm from https://elm-lang.org/
echo 2. Put the full path to elm.exe in elm_path.txt
echo    Example: e:\elm\0.19.1\bin\elm.exe
echo.
pause
exit /b 1

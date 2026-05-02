# Hex-Studio Build & Launch
# Usage: .\build.ps1                  -> build + start server + open
#        .\build.ps1 -BuildOnly       -> build only
#        .\build.ps1 -OpenOnly        -> start server + open (skip build)

param(
    [switch]$BuildOnly,
    [switch]$OpenOnly,
    [int]$Port = 8080
)

$ErrorActionPreference = "Stop"
$root = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $root

# Read elm path from config file
$elmPathFile = Join-Path $root "elm_path.txt"
if (-not (Test-Path $elmPathFile)) {
    Write-Host "[ERROR] elm_path.txt not found!" -ForegroundColor Red
    Write-Host "Create elm_path.txt with the path to elm.exe, e.g.:" -ForegroundColor Yellow
    Write-Host "  e:\elm\0.19.1\bin\elm.exe" -ForegroundColor Yellow
    exit 1
}
$elm = (Get-Content $elmPathFile -Raw).Trim()
if (-not $elm -or -not (Test-Path $elm)) {
    Write-Host "[ERROR] elm.exe not found at: $elm" -ForegroundColor Red
    exit 1
}

# === Build ===
if (-not $OpenOnly) {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host " Hex-Studio Build" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""

    $src = Join-Path $root "src\Main.elm"
    $out = Join-Path $root "gen\source.js"

    Write-Host "[Build] Compiling..." -ForegroundColor Yellow
    $result = & $elm make $src --output=$out 2>&1
    if ($LASTEXITCODE -ne 0) {
        Write-Host ""
        Write-Host "========================================" -ForegroundColor Red
        Write-Host " Build FAILED!" -ForegroundColor Red
        Write-Host "========================================" -ForegroundColor Red
        Write-Host $result
        pause
        exit 1
    }
    Write-Host "[OK] Build success!" -ForegroundColor Green
    Write-Host ""
}

if ($BuildOnly) { exit 0 }

# === Start HTTP Server ===
$url = "http://localhost:$Port"

# Find an available server
$serverType = $null
if (Get-Command python -ErrorAction SilentlyContinue) {
    $serverType = "python"
} elseif (Get-Command node -ErrorAction SilentlyContinue) {
    $serverType = "node"
} else {
    Write-Host "[WARN] Neither Python nor Node.js found." -ForegroundColor Yellow
    Write-Host "Opening with file:// protocol (may not work correctly)" -ForegroundColor Yellow
    Write-Host "Install Python or Node.js for proper local server." -ForegroundColor Yellow
    Start-Process (Join-Path $root "index.html")
    exit 0
}

Write-Host "========================================" -ForegroundColor Cyan
Write-Host " Hex-Studio Server" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Kill any process already using the port
$existing = netstat -ano 2>$null | Select-String ":$Port "
if ($existing) {
    $pidStr = ($existing -split '\s+')[-1]
    if ($pidStr -match '^\d+$') {
        Stop-Process -Id $pidStr -Force -ErrorAction SilentlyContinue
        Write-Host "[Port] Killed process on port $Port" -ForegroundColor Gray
    }
}

Write-Host "[Server] Starting on $url" -ForegroundColor Yellow
Write-Host "[Server] Press Ctrl+C to stop" -ForegroundColor Yellow
Write-Host ""

# Open browser after a short delay
Start-Sleep -Milliseconds 500
Start-Process $url
Write-Host "[OK] Browser opened at $url" -ForegroundColor Green
Write-Host ""

# Start the HTTP server (blocking)
if ($serverType -eq "python") {
    python -m http.server $Port
} elseif ($serverType -eq "node") {
    npx --yes serve "$root" --listen $Port --no-clipboard
}

# Build script for SendVarEdit on Windows.
# Auto-detects Visual Studio (even when vcvars*.bat are missing),
# sets up the MSVC + Windows SDK environment, runs configure.py and ambuild.
# Usage: powershell -ExecutionPolicy Bypass -File build.ps1 [-Arch x86|x86_64] [-ConfigureOnly]

param(
    [ValidateSet('x86', 'x86_64')]
    [string]$Arch = 'x86',
    [switch]$ConfigureOnly
)

$IsX64 = ($Arch -eq 'x86_64')
$HostBin = if ($IsX64) { 'Hostx64\x64' } else { 'Hostx64\x86' }
$LibSub = if ($IsX64) { 'x64' } else { 'x86' }

$ErrorActionPreference = 'Stop'
$ProjectRoot = $PSScriptRoot
$BuildDir = Join-Path $ProjectRoot 'build'
$AMBvswhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio\Installer\vswhere.exe'

function Resolve-VisualStudio {
    if (-not (Test-Path $AMBvswhere)) {
        throw "vswhere.exe not found at $AMBvswhere"
    }
    $path = & $AMBvswhere -latest -products * -property installationPath 2>$null
    if (-not $path) {
        throw 'Visual Studio installation not found'
    }
    return (Join-Path $path 'VC')
}

function Resolve-MsvcToolset([string]$VcRoot) {
    $toolsDir = Join-Path $VcRoot 'Tools\MSVC'
    if (-not (Test-Path $toolsDir)) {
        throw "MSVC toolset not found under $toolsDir"
    }
    $toolset = Get-ChildItem $toolsDir -Directory |
        Sort-Object Name -Descending |
        Where-Object {
            (Test-Path (Join-Path $_.FullName "bin\$HostBin\cl.exe")) -and
            (Test-Path (Join-Path $_.FullName 'include')) -and
            (Test-Path (Join-Path $_.FullName "lib\$LibSub"))
        } |
        Select-Object -First 1
    if (-not $toolset) {
        throw "No complete MSVC toolset (with cl.exe, include, lib\$LibSub) was found. Repair the 'Desktop development with C++' workload."
    }
    return $toolset.FullName
}

function Resolve-WindowsKit {
    $kitRoot = Join-Path ${env:ProgramFiles(x86)} 'Windows Kits\10'
    $version = Get-ChildItem (Join-Path $kitRoot 'Include') -Directory |
        Sort-Object Name -Descending |
        Select-Object -First 1
    if (-not $version) {
        throw "No Windows SDK found under $kitRoot"
    }
    return @{
        Root    = $kitRoot
        Version = $version.Name
    }
}

function Resolve-Ambuild {
    $candidates = @(
        (Join-Path $PSScriptRoot 'ambuild\build\ambuild\ambuild.exe'),
        (Join-Path (Split-Path (Get-Command python -ErrorAction SilentlyContinue).Source) 'ambuild.exe'),
        (Join-Path $HOME 'AppData\Roaming\Python\Python311\Scripts\ambuild.exe')
    )
    $candidates += Get-Command ambuild -ErrorAction SilentlyContinue | ForEach-Object { $_.Source }
    foreach ($c in $candidates) {
        if ($c -and (Test-Path $c)) {
            return $c
        }
    }
    # Fall back to the user scripts dir of whichever python is active.
    $scripts = & python -c "import sysconfig; print(sysconfig.get_path('scripts'))" 2>$null
    if ($scripts) {
        $path = Join-Path $scripts 'ambuild.exe'
        if (Test-Path $path) {
            return $path
        }
    }
    throw 'ambuild.exe not found. Install it with: pip install ./ambuild'
}

Write-Host '== SendVarEdit Windows build ==' -ForegroundColor Cyan
Write-Host "Project: $ProjectRoot"

# --- Resolve toolchain -----------------------------------------------------
$vcRoot = Resolve-VisualStudio
$toolset = Resolve-MsvcToolset $vcRoot
$kit = Resolve-WindowsKit
$ambuild = Resolve-Ambuild

$includePaths = @(
    (Join-Path $toolset 'include'),
    (Join-Path $kit.Root "Include\$($kit.Version)\ucrt"),
    (Join-Path $kit.Root "Include\$($kit.Version)\shared"),
    (Join-Path $kit.Root "Include\$($kit.Version)\um"),
    (Join-Path $kit.Root "Include\$($kit.Version)\winrt")
)
$libPaths = @(
    (Join-Path $toolset "lib\$LibSub"),
    (Join-Path $kit.Root "Lib\$($kit.Version)\ucrt\$LibSub"),
    (Join-Path $kit.Root "Lib\$($kit.Version)\um\$LibSub")
)

if ($IsX64) {
    $env:PATH = "$toolset\bin\Hostx64\x64;$toolset\bin\Hostx64\x86;" +
                "$($kit.Root)\bin\$($kit.Version)\x64;" +
                (Split-Path $ambuild) + ';' + $env:PATH
} else {
    $env:PATH = "$toolset\bin\Hostx64\x86;$toolset\bin\Hostx64\x64;" +
                "$($kit.Root)\bin\$($kit.Version)\x64;" +
                (Split-Path $ambuild) + ';' + $env:PATH
}
$env:INCLUDE = ($includePaths -join ';')
$env:LIB = ($libPaths -join ';')

Write-Host "VS:      $vcRoot" -ForegroundColor Green
Write-Host "Toolset: $toolset" -ForegroundColor Green
Write-Host "SDK:     $($kit.Version)" -ForegroundColor Green
Write-Host "AMBuild: $ambuild" -ForegroundColor Green
Write-Host "Arch:    $Arch" -ForegroundColor Green

# --- Configure -------------------------------------------------------------
if (-not (Test-Path $BuildDir)) {
    New-Item -ItemType Directory -Path $BuildDir | Out-Null
}
Push-Location $BuildDir
try {
    Write-Host ''
    Write-Host '== Running configure.py ==' -ForegroundColor Cyan
    & python ..\configure.py `
        --mms-path="..\..\mmsource" `
        --sm-path="..\..\sourcemod" `
        --hl2sdk-manifest-path="..\..\sourcemod\hl2sdk-manifests" `
        --hl2sdk-root="..\.." `
        --sdks=present `
        --targets=$Arch `
        --enable-optimize
    if ($LASTEXITCODE -ne 0) {
        throw "configure.py failed with exit code $LASTEXITCODE"
    }

    if ($ConfigureOnly) {
        Write-Host ''
        Write-Host 'Configure done. Run ambuild manually.' -ForegroundColor Yellow
        return
    }

    Write-Host ''
    Write-Host '== Running ambuild ==' -ForegroundColor Cyan
    & $ambuild
    if ($LASTEXITCODE -ne 0) {
        throw "ambuild failed with exit code $LASTEXITCODE"
    }

    Write-Host ''
    Write-Host '== Build succeeded ==' -ForegroundColor Green
    Write-Host "Output: $(Join-Path $BuildDir 'package')"
}
finally {
    Pop-Location
}

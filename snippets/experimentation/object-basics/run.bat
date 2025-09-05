echo "[*] compilation start..."
if exist "artefact\app.exe" del  "artefact\app.exe"
pbuild

set /p answer=""
artefact\app.exe
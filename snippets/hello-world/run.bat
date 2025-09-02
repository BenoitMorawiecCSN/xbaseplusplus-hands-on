echo "[*] compilation start..."
xpp code.prg
alink code.obj
del code.obj

echo "[+] compilation ended"
set /p answer=""
cls
code.exe
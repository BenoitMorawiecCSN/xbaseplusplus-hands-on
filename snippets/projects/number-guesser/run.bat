echo "[*] compilation start..."
xpp number_guesser.prg
alink number_guesser.obj
del number_guesser.obj

echo "[+] compilation ended"
set /p answer=""
cls
number_guesser.exe
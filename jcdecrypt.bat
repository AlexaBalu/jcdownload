@echo off

set /p folderToDecrypt=Folder to decrypt: 

java -cp jcdownload.jar info.cemu.download.Decrypt PUT_HERE_COMMON_KEY "%folderToDecrypt%"

pause
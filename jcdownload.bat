@echo off

set /p titleKey=Title key or folder of resume: 

java -jar jcdownload.jar PUT_HERE_COMMON_KEY "%titleKey%"

pause
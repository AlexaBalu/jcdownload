@echo off

set /p titleKey=Title Key or folder of resume: 

java -jar jcdownload.jar PUT_HERE_COMMON_KEY "%titleKey%"

pause
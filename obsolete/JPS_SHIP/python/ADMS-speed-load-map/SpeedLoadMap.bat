@echo off

call .\env\Scripts\python.exe ADMS-Map-SpeedTorque-NOxSoot.py

if %ERRORLEVEL% neq 0 goto ProcessError
exit /b 0

:ProcessError
echo Please follow README.TXT setup instructions before running the SLM.
exit /b 1

@echo off
:main
del /f "Output file.csv"
python open_and_save_csv.pyw
fc "Output file.csv" "Output reference.csv" > nul
if errorlevel 1 goto error

:next
echo success
goto success

:error
echo failed check

:success
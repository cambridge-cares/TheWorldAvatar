@echo off
rem Part ot ontozeolite package.
rem Preparation of crystal information (CIF) knowledge graphs.
rem Author: Rutkevych Pavlo
rem Date: 2024/04/01

set PYTHON=python
set SRCDIR=python/

if "%~1"=="" (
    echo Missing command in crystal.bat. Expect one of: data, csv, owl.
    goto EXIT
) else (
    set COMMAND=%~1
)

if "%~2"=="" (
    echo Missing datadirectory in crystal.bat. 
    goto EXIT
) else (
    set DATADIR=%~2
)

if %COMMAND%==data (
  goto DATA
)

if %COMMAND%==csv (
  goto CSV
)

if %COMMAND%==owl (
  goto OWL
)

:DATA
echo Generate crystal data
rem %PYTHON% %SRCDIR%check_final.py %DATADIR%

goto EXIT

:CSV
echo Generate crystal csv
%PYTHON% %SRCDIR%crystalinfo.py %DATADIR%

goto EXIT

:OWL
echo Generate crystal owl

echo %
if not exist %DATADIR%\crystal\owl (
  md %DATADIR%\crystal\owl
)

set RANGE=(0,1,64)
setlocal enabledelayedexpansion
for /L %%i in %RANGE% do (
  csv2rdf %DATADIR%\crystal\csv\cif_twa_%%i.csv --csvType=abox
  sleep 1
  move %DATADIR%\crystal\csv\cif_twa_%%i.csv.owl  %DATADIR%\crystal\owl\cif_twa_%%i.csv.owl > nul
)

goto EXIT

:EXIT

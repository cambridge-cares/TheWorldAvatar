@echo off
rem Part ot ontozeolite package.
rem Preparation of crystal information (CIF) knowledge graphs.
rem Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
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
%PYTHON% %SRCDIR%check_ciffiles.py %DATADIR%

goto EXIT

:CSV
echo Generate crystal csv
rem %PYTHON% %SRCDIR%crystalinfo.py %DATADIR%
%PYTHON% %SRCDIR%crystalinfo.py --cif=ontozeolite\crystal\data\cif_list.csv --outDir=ontozeolite\crystal\csv\ --xrdDir=ontozeolite\crystal\xrd --abox-prefix=https://www.theworldavatar.com/kg/ontozeolite/

goto EXIT   

:OWL
echo Generate crystal owl

echo %
if not exist %DATADIR%\crystal\owl (
  md %DATADIR%\crystal\owl
)

set RANGE=(0,1,134)
setlocal enabledelayedexpansion
for /L %%i in %RANGE% do (
  if exist %DATADIR%\crystal\csv\cif_twa_%%i.csv (
    csv2rdf %DATADIR%\crystal\csv\cif_twa_%%i.csv --csvType=abox
    rem sleep 1
    move %DATADIR%\crystal\csv\cif_twa_%%i.csv.owl  %DATADIR%\crystal\owl\cif_twa_%%i.csv.owl > nul
  )
)

goto EXIT

:EXIT

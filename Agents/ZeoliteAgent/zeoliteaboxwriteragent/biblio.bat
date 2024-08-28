@echo off
rem Part of ontozeolite package.
rem Generation of bibliography data for ontozeolites.
rem Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
rem Date: 2024/04/01

set PYTHON=python
set SRCDIR=python/

if "%~1"=="" (
    echo Missing command in biblio.bat. Expect one of: data, csv, owl.
    goto EXIT
) else (
    set COMMAND=%~1
)

if "%~2"=="" (
    echo Missing datadirectory in biblio.bat. 
    set DATADIR=ontozeolite
    rem goto EXIT
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
rem echo Preprocessing biblio data:
%PYTHON% %SRCDIR%/combine_bib.py %DATADIR%

goto EXIT

:CSV
rem echo Generate biblio csv:
mkdir %DATADIR%\biblio\csv\
%PYTHON% %SRCDIR%/bib2csv.py %DATADIR%

goto EXIT

:OWL
rem echo Generate biblio owl:
csv2rdf %DATADIR%/biblio/csv/onto_bib.csv --csvType=abox 
mkdir %DATADIR%\biblio\owl\
move %DATADIR%\biblio\csv\onto_bib.csv.owl %DATADIR%\biblio\owl\.

goto EXIT

:EXIT

@echo off
rem Part of ontozeolite package.
rem Generation of zeolite-specific data.
rem Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
rem Date: 2024/04/01

set PYTHON=python
set SRCDIR=python/

if "%~1"=="" (
    echo Missing command in zeolite.bat. Expect one of: data, csv, owl.
    goto EXIT
) else (
    set COMMAND=%~1
)

if "%~2"=="" (
    echo Missing data directory in zeolite.bat. Exiting.
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
rem echo Generate zeolite data
rem Do nothing

goto EXIT

:CSV
echo Generate zeolite csv
md %DATADIR%\zeolite\csv

md %DATADIR%\zeolite\csv\100
python %SRCDIR%\csv_maker.py -c all -f   0 -t 100 -o %DATADIR%\zeolite\csv\100
python %SRCDIR%csv_merger.py %DATADIR%\zeolite\csv\100

md %DATADIR%\zeolite\csv\200
python %SRCDIR%\csv_maker.py -c all -f 100 -t 200 -o %DATADIR%\zeolite\csv\200
python %SRCDIR%csv_merger.py %DATADIR%\zeolite\csv\200

md %DATADIR%\zeolite\csv\300
python %SRCDIR%\csv_maker.py -c all -f 200 -t 300 -o %DATADIR%\zeolite\csv\300
python %SRCDIR%csv_merger.py %DATADIR%\zeolite\csv\300

goto EXIT

:OWL
rem echo Generate zeolite owl
mkdir %DATADIR%\zeolite\owl\

csv2rdf %DATADIR%/zeolite/csv/100/all.csv --csvType=abox
move %DATADIR%\zeolite\csv\100\all.csv.owl %DATADIR%\zeolite\owl\ontozeolite_kg_01.owl

csv2rdf %DATADIR%/zeolite/csv/200/all.csv --csvType=abox
move %DATADIR%\zeolite\csv\200\all.csv.owl %DATADIR%\zeolite\owl\ontozeolite_kg_02.owl

csv2rdf %DATADIR%/zeolite/csv/300/all.csv --csvType=abox
move %DATADIR%\zeolite\csv\300\all.csv.owl %DATADIR%\zeolite\owl\ontozeolite_kg_03.owl

goto EXIT

:EXIT

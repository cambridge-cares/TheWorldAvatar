@echo off
rem Part ot ontozeolite package.
rem Preparation of OntoZeolite Knowledge Graph
rem Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
rem Date: 2024/04/01

set PYTHON=python
set SRCDIR=python/
set DATADIR=ontozeolite

rem Blazegraph server and namespace:
rem For example, local computer:
set SERVER=http://localhost:8080/blazegraph/namespace/
set NAMESPACE=zeo06k
set AUTHFILE=
rem For example: remove server:
rem set SERVER=http://178.128.105.213:3838/blazegraph-dev/ui/namespace/
rem set NAMESPACE=ontozeo
rem set AUTHFILE=blazedev.auth

if "%~1"=="" (
    rem Default = preparation of data.
    rem Do nothing.
) else (
    if "%~1"=="test" (
        goto UNITTEST
    ) else (
        echo Unknown command line argument %~1
    )
)

rem goto START

rem Bibliography *.bib to .csv file:
call biblio.bat data %DATADIR%
call biblio.bat csv  %DATADIR%
rem %PYTHON% %SRCDIR%/bib2csv.py %DATADIR%

rem Bibliography .csv to .owl file:
call biblio.bat owl %DATADIR%

rem Crystal data *.cif to .csv files:
call crystal.bat data %DATADIR%
call crystal.bat csv  %DATADIR%

rem Crystal data *.csv to .owl files:
call crystal.bat owl %DATADIR%

rem Zeolite-specific data *.json to .csv files:
call zeolite.bat data %DATADIR%
call zeolite.bat csv  %DATADIR%

rem Zeolite-specific data *.csv to .owl files:
call zeolite.bat owl %DATADIR%

:START
rem Upload all data to Blazegraph server:
call upload_cryst.bat  %SERVER% %NAMESPACE% %AUTHFILE%

rem Testing data on the server:
echo Need to run tests of data on server

goto EXIT

:UNITTEST
echo Need to run unittests for python code

goto EXIT
:EXIT


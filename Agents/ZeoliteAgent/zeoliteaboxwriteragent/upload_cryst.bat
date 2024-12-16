@echo off
rem Part of ontozeolite package.
rem Generation of entire ontozeolite knowledge graph.
rem Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
rem Date: 2024/04/01

rem Uploader to a blazegraph server. Requires vertual environment pyuploader_venv.
rem https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_uploader
rem
rem Accepts up to THREE command line arguments: server namespace auth-file.
rem
rem It may take a long time to upload files to a remove server.
rem The start and end time stamps can be saved to the upload.log file.
rem The command line arguments:
rem     - the blazegraph server, for example
rem       http://localhost:8080/blazegraph/namespaces/
rem     - the namespace in blazegraph
rem     - (optional) the authorization file for the server,
rem                  which is a txt file with one line: username:password

if "%1"=="" (
  echo "Missing blazegraph server name"
  rem set BASE=http://localhost:8080/blazegraph/namespaces/
  rem set NS=ontozeo
  rem set AUTH=--no-auth
  goto EXIT
) else (
  if "%2"=="" (
    echo "Missing blazegraph server name"
  ) else (
    set BASE=%1
    set NS=%2
    if "%3"=="" (
      set AUTH=--no-auth
    ) else (
      set AUTH=%3
    )
  )
)

set URL=%BASE%%NS%/sparql
echo Uploading to %URL% %AUTH%

rem set PREFIX=start
set PREFIX=

set OWL_DIR=ontozeolite\zeolite\owl
for /L %%i in (1,1,3) do (
  %PREFIX% pyuploader ts_upload %OWL_DIR%"\ontozeolite_kg_0"%%i".owl" --url=%URL% %AUTH%
)
rem goto EXIT

set OWL_DIR=ontozeolite\biblio\owl
%PREFIX% pyuploader ts_upload %OWL_DIR%"\onto_bib.csv.owl" --url=%URL% %AUTH%

set OWL_DIR=ontozeolite\crystal\owl
for /L %%i in (0,1,128) do (
  %PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_"%%i".csv.owl" --url=%URL% %AUTH%
)
goto EXIT

:EXIT
rem echo Ended upload: %DATE% %TIME% >> upload.log
echo Ended upload: %DATE% %TIME%

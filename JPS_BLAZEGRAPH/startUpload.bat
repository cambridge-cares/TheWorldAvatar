@ECHO OFF
set arg1=%1
set arg2=%2
set arg3=%3
echo For uploading ontologies from a directory provide the URL of Endpoint, name of repository and absolute path of the ontology directory as follows: http://localhost:8080/blazegraph ontokin C:/data/kb
:loop
for /f "tokens=*" %%f in ('call ontochemUpload.bat %arg1% %arg2% %arg3%') do set lastLine=%%f
if "%lastLine%"=="Now the tool will stop. Run it again to finish the import." (
	goto loop
)
echo Finished OWL/RDF file upload

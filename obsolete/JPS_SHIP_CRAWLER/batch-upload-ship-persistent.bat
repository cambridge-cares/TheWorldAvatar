@echo off
setlocal enabledelayedexpansion
set cnt=0
for %%i in (C:\TOMCAT\webapps\ROOT\kb\ships\*) do (
	set file=%%~NXi
	if !file:~0^,4!==Ship (
		set /a cnt+=1
		echo !cnt!
		echo %%~NXi
		curl -X POST -H Content-Type:application/rdf+xml -T %%i -G http://localhost/damecoolquestion/ships-persistent/data -d default
	)
)
echo %cnt%

endlocal

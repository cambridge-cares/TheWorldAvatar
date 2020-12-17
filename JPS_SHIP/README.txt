A Python Virtual Environment will now be created during 'mvn clean install' on the JPS_SHIP project. 

To use this virtual environment for testing, please do the following:
1) cd to JParkSimulator-git\JPS_SHIP\python\ADMS-speed-load-map in your workarea.
2) Execute the command 'pipenv run python'.
3) Once you're in the python shell, execute the following commands:
	import sys
	sys.executable
4) You should now see a path to where the python is executed from the virtual environment. 
5) Copy the path, excluding '\\Scripts\\python.exe' on Windows or 'bin/python' on Unix-like OS.
6) Replace the value of speed.load.map.venv.dir in jpstest.properties located in JPS_BASE_LIB.
	e.g. speed.load.map.venv.dir = C:\\Users\\RFOO01\\.virtualenvs\\ADMS-speed-load-map-KcR3PzvU
7) Run 'mvn clean install' on JPS_BASE_LIB.

